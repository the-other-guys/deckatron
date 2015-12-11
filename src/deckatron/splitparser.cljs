(ns deckatron.splitparser
  (:require
    [clojure.string :as str]
    [cljs.test :refer-macros [deftest testing is run-tests]]))



(enable-console-print!)

;;
;; txt
;;  -> [blocks] <:slide|:note>
;;  -> [paragraphs] <:code|:olist|:ulist|:text>
;;  -> [lines]
;;  -> [spans] <:code|:em|:strong|:strikethrough|:text>
;;
;;

;; Rules, grammar and priority:
;; 1. Slides:
;; - are separated by #"===\n\n"
;; - have split priority #1
;; - if text has no slide/note separator – slide one is assumed.
;; 2. Notes:
;; - are separated by #"---\n\n"
;; - have split priority #2
;; 3. Multiline code blocks:
;; - are separated by #"\n``` "
;; - have split priority #3
;; - each even split chunk is a code block
;; - first line (word) of a codeblock is a language name


(def ^:private SLIDES-SEPARATOR "\n===\n\n")
(def ^:private SLIDES-SEPARATOR-RE (re-pattern SLIDES-SEPARATOR))
(def ^:private NOTES-SEPARATOR "\n---\n\n")
(def ^:private NOTES-SEPARATOR-RE (re-pattern NOTES-SEPARATOR))
(def ^:private CODE-SEPARATOR "```")
(def ^:private CODE-SEPARATOR-RE (re-pattern CODE-SEPARATOR))
(def ^:private PARAGRAPH-SEPARATOR "\n\n")
(def ^:private PARAGRAPH-SEPARATOR-RE (re-pattern PARAGRAPH-SEPARATOR))



(defn- ->raw-page [s type]
  (when-not (str/blank? s)
    {:s/type type :s/text (str/trim-newline s)}))

(defn ->raw-pages [s]
  (let [s  (str SLIDES-SEPARATOR s "\n\n")
        ss (->> (str/split s NOTES-SEPARATOR-RE)
                (mapv #(str/split % SLIDES-SEPARATOR-RE)))
        f (fn [[note & slides]]
            [(->raw-page note :note)
             (mapv #(->raw-page % :slide) slides)])
        raw-blocks (->> ss (mapv f) flatten (remove nil?) vec)]
    raw-blocks))


;[{:p/type :text, :p/lines [{:l/elements [{:e/text "foo ", :e/types #{}}
;                                         {:e/text "bar baz", :e/types #{:em}}
;                                         {:e/text " ", :e/types #{}}
;                                         {:e/text "qux", :e/types #{:em}}]}]}])))

(defn- ->span [s modifiers]
  {:e/text s :e/types (set modifiers)})

(defn- ->paragraph [type lines]
  {:p/type type :p/lines lines})

(defn- ->raw-line [s]
  {:l/elements [(->span s [])]})


(defn- span-tag-info [sep rsep tag s]
  (let [chunks  (str/split s rsep)
        start   (-> chunks first count)
        endsw   (str/ends-with? s sep)
        c       (count chunks)
        chunks  (cond
                  (and (= c 2) (not endsw))
                  [s]
                  (and (even? c) (not endsw))
                  (let [merged-2-last-els (str/join sep (subvec chunks (- c 2)))]
                    (conj (pop (pop chunks)) merged-2-last-els))
                  :else chunks)
        present (or (> c 2) (and endsw (= c 2)))
        idxs    (filterv odd? (range c))
        idxs    (if (or (odd? c) endsw) idxs (pop idxs))]
    {:present present
     :chunks chunks
     :start start
     :sep sep
     :tag tag
     :endswith endsw
     :idxs (set idxs)}))



(def ^:private tag-fns-by-priority
  {1 [(partial span-tag-info "**" #"\*\*" :strong)
      (partial span-tag-info "__" #"__"   :strong)
      (partial span-tag-info "`"  #"`"    :code)]
   2 [(partial span-tag-info "*"  #"\*"   :em)
      (partial span-tag-info "_"  #"_"    :em)
      (partial span-tag-info "~"  #"~"    :strikethrough)]})

(defn- parse-line
  ([s]
   (parse-line s 1 []))
  ([s priority tags]
  (when-not (= s "")
    (let [candidate-fns (tag-fns-by-priority priority)
          candidates    (if (nil? candidate-fns)
                          nil
                          (map #(% s) candidate-fns))
          winner (->> candidates
                   (filter :present)
                   (sort-by :start)
                   first)
          f (fn [i s ti]
              (let [tags (if (contains? (:idxs ti) i)
                           (cons (:tag ti) tags)
                           tags)]
                (parse-line s priority tags)))
          use (fn [ti] (->> (:chunks ti) (map-indexed #(f %1 %2 ti)) vec))]
      (print priority)
      (print candidates)
      (print winner)
      (cond
        (nil? candidates)  [(->span s tags)]
        (nil? winner)      (parse-line s (inc priority) tags)
        :else              (use winner))))))


(defn- ->line [s]
  (let [spans (->> s parse-line flatten (remove nil?) vec)]
    {:l/elements spans}))

(defn- ->code-paragraph [s]
  (let [[lang-line & code-lines] (str/split s #"\n")
        lang (str/trim lang-line)
        p (->paragraph :code (mapv ->raw-line code-lines))]
    (assoc p :p/language lang)))

(defn- ->header [s type]
  (let [txt (-> s (str/split #" " 2) second)
        lines [(->raw-line txt)]]
    (->paragraph type lines)))

(defn- ->text-paragraph [ss]
  (->> ss (mapv ->line) (->paragraph :text)))

(defn- ->unordered-list [ss]
  (->> ss (mapv ->line) (->paragraph :unordered-list)))

(defn- ->ordered-list [ss]
  (->> ss (mapv ->line) (->paragraph :ordered-list)))

(defn- unordered-list? [lines sep]
  (->> lines
    (map #(str/starts-with? % sep))
    (every? true?)))

(defn- digit-with-dot? [s]
  (->> s (re-matches #"(\d+\.).*") empty? not))

(defn- ordered-list? [lines]
  (->> lines
    (map #(-> % (str/split #" ") first))
    (map digit-with-dot?)
    (every? true?)))

(defn- parse-paragraph [s]
  (when-not (str/blank? s)
    (let [[h t] (str/split s #" " 2)]
      (condp = h
        "#"    (->header t :h1)
        "##"   (->header t :h2)
        "###"  (->header t :h3)
        "####" (->header t :h4)
        "*"    (->unordered-list t)
        "-"    (->unordered-list t)
        (let [lines (str/split-lines s)]
          (cond
            (unordered-list? lines "*") (->unordered-list lines)
            (unordered-list? lines "-") (->unordered-list lines)
            (ordered-list?   lines)     (->ordered-list   lines)
            :else                       (->text-paragraph lines)))))))

(defn- ->paragraphs [s]
  (let [ss (str/split s CODE-SEPARATOR-RE)
        f (fn [i segment]
            (if (odd? i)
              (->code-paragraph segment)
              (map parse-paragraph (str/split segment PARAGRAPH-SEPARATOR-RE))))]
    (->> ss (map-indexed f) flatten (remove nil?) vec)))

(defn ->page [p]
  (assoc p :s/paragraphs (->paragraphs (:s/text p))))

(defn parse [s]
  (->> s ->raw-pages (mapv ->page)))



;;;; TESTS

(def ^:private slide-line "slide line")
(def ^:private note-line "note line")
(def ^:private slide-lines (str/join "\n" [slide-line slide-line]))
(def ^:private note-lines (str/join "\n" [note-line note-line]))
(def ^:private slide-p (str SLIDES-SEPARATOR slide-lines))
(def ^:private note-p (str NOTES-SEPARATOR note-lines))
(def ^:private note {:s/type :note :s/text note-lines})
(def ^:private slide {:s/type :slide :s/text slide-lines})
(def ^:private code-line "code line")
(def ^:private code-lang "clojure")
(def ^:private code-lines (str/join "\n" [code-line code-line]))
(def ^:private code-p (str CODE-SEPARATOR code-lang "\n" code-lines code-lines))


(deftest test-split-text-into-slides
  (is (= (->raw-pages
           (str/join "\n"
             [note-p slide-p note-p slide-p slide-p]))
        [note slide note slide slide]))

  (is (= (->raw-pages
           (str/join "\n"
             [slide-p note-p note-p slide-p slide-p]))
        [slide note note slide slide]))

  (is (= (->raw-pages
           (str/join "\n"
             [slide-lines note-p note-p slide-p slide-p]))
        [slide note note slide slide])))


; aaaa[key](https://w)bbb[note](https://w)ccc
; => ["aaa" "key](https://w)bbb" "note](https://w)ccc"] ;; rest
; => ["aaa" ["key" "https://w)bbb"] ["note" "https://w)ccc"]]
; => ["aaa" ["key" ["https://w" "bbb"] ["note" ["https://w" "ccc"]]]
; => ["aaa" ["key" "https://w"] "bbb" ["note" "https://w"] "ccc"]



(deftest test-parse
  (is (= (->paragraphs "line```clojure\n(+ 1 1)```yoyoyo")
        [{:p/type :text, :p/lines [{:l/elements [{:e/text "line", :e/types #{}}]}]}
         {:p/type :code, :p/lines [{:l/elements [{:e/text "(+ 1 1)", :e/types #{}}]}], :p/language "clojure"}
         {:p/type :text, :p/lines [{:l/elements [{:e/text "yoyoyo", :e/types #{}}]}]}])))


(deftest test-parse-inline-img
  (is (= (->paragraphs "![kfc gif](http://media.giphy.com/media/3jps6E3j2VlsI/giphy.gif)")
        [{:p/type :text, :p/lines [{:l/elements [{:e/text "kfc gif", :e/types #{:image}
                                                  :e/href "http://media.giphy.com/media/3jps6E3j2VlsI/giphy.gif"}]}]}])))

(deftest test-parse-url
  (is (= (->paragraphs "[key note](https://www.youtube.com/watch?v=FihU5JxmnBg)")
        [{:p/type :text, :p/lines [{:l/elements [{:e/text "key note", :e/types #{:link}
                                                  :e/href "https://www.youtube.com/watch?v=FihU5JxmnBg"}]}]}])))
;(run-tests)