(ns deckatron.parser
  (:require
    [instaparse.core :as insta]
    [clojure.string :as string]
    [cljs.test :refer-macros [deftest testing is run-tests]]))

(enable-console-print!)


(def ^:private SLIDES-SEPARATOR #"===\n")
(def ^:private COMMENTS-SEPARATOR #"---\n")

(def ^:private parser
  ;;https://github.com/chameco/Hitman/blob/master/src/hitman/core.clj#L9
  (insta/parser
    "<Blocks> = (EOL | Paragraph | Header | List | Ordered | Code | CodeBlock | BlockQuote)+
    CodeBlock = CodeMarker Whitespace+ CodeLanguage EOL CodeLine+ CodeMarker EOL
    CodeLanguage = LineChar*
    <CodeMarker> = '```'
    CodeLine = LineChar* EOL
    Header = Headermarker Whitespace+ Span EOL
    <Headermarker> = ('#' | '##' | '###' | '####')
    BlockQuote = QuoteLine+ Blankline+
    QuoteLine = QuoteMarker Whitespace+ LineChar* EOL
    <QuoteMarker> = '>'
    List = Listline+ Blankline+
    Listline = Listmarker Whitespace+ Span EOL
    <Listmarker> = <'+' | '*' | '-'>
    Ordered = Orderedline+ Blankline+
    Orderedline = Orderedmarker Whitespace+ Span EOL
    <Orderedmarker> = <#'[0-9]+\\.'>
    Code = Codeline+ Blankline+
    Codeline = <Space Space Space Space> Span EOL
    Paragraph = Span+ Blankline+
    Span = (LineChar | Strike | Code | Em | Strong | Link | Image)+
    Image = '![' Name '](' URL ')'
    Link = '[' Name '](' URL ')'
    Name = LineChar+
    URL = LineChar+
    Strong = ('**' Span '**' | '__' Span '__')
    Code = '`' Span '`'
    Strike = ('~' Span '~' | '-' Span '-')
    Em = ('*' Span '*' | '_' Span '_')
    <Blankline> = Whitespace* EOL
    <LineChar> = #'[^\\n]'
    <Whitespace> = #'(\\ | \\t)+'
    <Space> = ' '
    <EOL> = <'\\n'>"))

(defn- ->element [s & types]
  (if (== (count s) 0) []
    [{:e/text s :e/types (set (flatten types))}]))

(defn- ->ref-element [s l types]
  (if (== (count s) 0) []
    [{:e/text s :e/href l :e/types (set (flatten types))}]))

(defn- ->paragraph [type elements]
  {:p/type type :p/lines elements})

(defn- ->code-paragraph [type language elements]
  {:p/type type :p/language language :p/lines elements})

(defn- reduce-spans [ss]
  (let [f (fn [m1 m2]
            (update m1 :text str (:text m2)))
        by-types (partition-by :e/types ss)]
    (mapv #(reduce f %) by-types)))

(defn- parse-list-line [line]
  (->> (rest line)
;       (map parse-span)
       reduce-spans))

(defn- parse-list [block]
  "'* one\n* two\n\n'
  ==>
  [:List
   [:Listline \" \" \"one\"]
   [:Listline \" \" \"two\"]]"
  (let [lines (mapv parse-list-line (rest block))]
    (->paragraph :list lines)))


(defn- parse-ordered-list [block]
  (assoc (parse-list block) :p/type :ordered-list))

(defn- span? [s]
    (if (vector? s)
      (if (= (first s) :Span) (string? (second s)) false)
      false))

(defn- concat-spans [a b]
    [:Span (+ (string/join (rest a)) (string/join (rest b)))])

(defn- concat-samelevel-spans [b]
  (if (and (sequential? b) (> (count b) 1))
    (if (and (span? (first b)) (span? (second b)))
        (concat-samelevel-spans (vec (concat [(concat-spans (first b) (second b))] (rest (rest b)))))
        (vec (concat (first b) (concat-samelevel-spans (rest b)))))
        b))

(defn- span-take [elms acc]
  (if (> (count elms) 0)
    (if (string? (first elms)) (span-take (rest elms) (+ acc (first elms))) acc)
    acc))

(defn- span-rest [elms]
  (if (> (count elms) 0)
    (if (string? (first elms)) (span-rest (rest elms) elms) elms)
    elms))

(defn- remove-key [elms]
  (string/join (rest elms)))

(defn- reduce-elements [elms tags]
    (if (> (count elms) 0)
        (case (first elms)
              :Span (concat (->element (span-take (rest elms) "") tags)
                      (reduce-elements (span-rest (rest elms)) tags))
              :Em   (reduce-elements (nth elms 2) (concat tags [:em]))
              :Code (reduce-elements (nth elms 2) (concat tags [:code]))
              :Strike(reduce-elements (nth elms 2) (concat tags [:strike]))
              :Strong (reduce-elements (nth elms 2) (concat tags [:strong]))
              :Image (->ref-element (remove-key (nth elms 2)) (remove-key (nth elms 4)) [:image])
              :Link (->ref-element (remove-key (nth elms 2)) (remove-key (nth elms 4)) [:link])
              (concat (reduce-elements (first elms) tags)
                (if (> (count (rest elms)) 0) (reduce-elements (concat [:Span] (rest elms)) tags) [])))
        []))

(defn- reduce-res [elms]
  [{:l/elements (reduce-elements elms [])}])

(defn- list-lines [elms]
  (mapcat #(reduce-res (vec (rest (rest %)))) elms))

(defn- quote-line [letters]
  [{:l/elements (->element (string/join letters) [])}])

(defn- quote-lines [elms]
  (mapcat #(quote-line (drop 3 %)) elms))

(defn- code-lines [elms]
  (mapcat #(quote-line (rest %)) elms))

(defn- lang-name [elms]
  (string/join (rest elms)))

(defn- header-key [q]
  (case q
        "#" :h1
        "##" :h2
        "###" :h3
        "####" :h4
        :h5))

(defn- reduce-block [b]
    (case (first b)
      :Span (->paragraph :text (reduce-res b))
      :Ordered (->paragraph :ordered-list (list-lines (rest b)))
      :List (->paragraph :unordered-list (list-lines (rest b)))
      :Header (->paragraph (header-key (second b)) (reduce-res (nth b 3)))
      :BlockQuote (->paragraph :blockquote (quote-lines (rest b)))
      :CodeBlock (->code-paragraph :code (lang-name (nth b 3)) (code-lines (drop-last (drop 4 b))))
      :Paragraph (->paragraph :text (reduce-res (concat-samelevel-spans (vec (rest b)))))
      nil))

(defn- reduce-blocks [blocks]
  (if (sequential? (first blocks))
    (for [b blocks] (reduce-block b))
    (reduce-block blocks)))

(defn parse [s]
  (->> s parser vec reduce-blocks))


(defn split-text-into-slides [t]
  "by default, top page - is a slide, not comment, even if there is no separator."
  (clojure.string/split (string/join [t "\n"]) SLIDES-SEPARATOR))

;  (let [t (str "===\n" t)
;        pages (->> (clojure.string/split t COMMENTS-SEPARATOR)
;                   (mapv #(clojure.string/split % SLIDES-SEPARATOR)))
;        f (fn [[c & ss]]
;            [{:s/type :comment :s/text c}
;             (mapv #(into {} {:s/type :slide :s/text %}) ss)])]
;    (->> pages
;         (map f)
;         flatten
;         (remove #(clojure.string/blank? (:s/text %)))
;         vec)))

;; TESTS


(deftest test-parse-inline-img
  (is (= (parse "![kfc gif](http://media.giphy.com/media/3jps6E3j2VlsI/giphy.gif)\n\n")
         [{:p/type :text, :p/lines [{:l/elements [{:e/text "kfc gif", :e/types #{:image}
                                                   :e/href "http://media.giphy.com/media/3jps6E3j2VlsI/giphy.gif"}]}]}])))

(deftest test-parse-url
  (is (= (parse "[key note](https://www.youtube.com/watch?v=FihU5JxmnBg)\n\n")
         [{:p/type :text, :p/lines [{:l/elements [{:e/text "key note", :e/types #{:link}
                                                   :e/href "https://www.youtube.com/watch?v=FihU5JxmnBg"}]}]}])))
(deftest test-parse-simple
  (is (= (parse "hsj ajkds ashjdk\n")
         [{:p/type :text, :p/lines [{:l/elements [{:e/text "hsj ajkds ashjdk", :e/types #{}}]}]}])))

(deftest test-parse-heading-1
  (is (= (parse "# foo + *bar*!\n\n")
         [{:p/type :h1, :p/lines [{:l/elements [{:e/text "foo + ", :e/types #{}} 
                                  {:e/text "bar", :e/types #{:em}}
                                  {:e/text "!", :e/types #{}}]}]}])))

(deftest test-parse-heading-2
  (is (= (parse "## foo + bar baz_?\n\n")
         [{:p/type :h2, :p/lines [{:l/elements [{:e/text "foo + bar baz_?", :e/types #{}}]}]}])))

(deftest test-parse-heading-3
  (is (= (parse "### just foo\n\n")
         [{:p/type :h3, :p/lines [{:l/elements [{:e/text "just foo", :e/types #{}}]}]}])))

(deftest test-parse-heading-4
  (is (= (parse "#### just foo\n\n")
         [{:p/type :h4, :p/lines [{:l/elements [{:e/text "just foo", :e/types #{}}]}]}])))

(deftest test-parse-block-w-plain-text
  (is (= (parse "foo *bar!\n")
         [{:p/type :text, :p/lines [{:l/elements [{:e/text "foo *bar!", :e/types #{}}]}]}])))

(deftest test-parse-block-w-modified-text
  (is (= (parse "foo *bar baz* _qux_\n")
         [{:p/type :text, :p/lines [{:l/elements [{:e/text "foo ", :e/types #{}}
                                                  {:e/text "bar baz", :e/types #{:em}}
                                                  {:e/text " ", :e/types #{}}
                                                  {:e/text "qux", :e/types #{:em}}]}]}])))

(deftest test-parse-block-w-nested-modified-text
  (is (= (parse "foo __*bar baz*__ _qux_.\n")
         [{:p/type :text, :p/lines [{:l/elements [{:e/text "foo ", :e/types #{}}
                                                  {:e/text "bar baz", :e/types #{:em :strong}}
                                                  {:e/text " ", :e/types #{}}
                                                  {:e/text "qux", :e/types #{:em}}
                                                  {:e/text ".", :e/types #{}}]}]}])))


(deftest test-parse-2-ordered-lists-w-modified-text
    (is (= (parse (str "1. foo **bar baz**\n"
                       "2. foo __bar baz__\n\n"
                       "1. foo ~bar baz~\n"
                       "2. foo `bar baz`\n\n"))
           [{:p/type :ordered-list, :p/lines [{:l/elements [{:e/text "foo ", :e/types #{}}
                                                            {:e/text "bar baz", :e/types #{:strong}}]}
                                              {:l/elements [{:e/text "foo ", :e/types #{}}
                                                            {:e/text "bar baz", :e/types #{:strong}}]}]}
            {:p/type :ordered-list, :p/lines [{:l/elements [{:e/text "foo ", :e/types #{}}
                                                            {:e/text "bar baz", :e/types #{:strike}}]}
                                              {:l/elements [{:e/text "foo ", :e/types #{}}
                                                            {:e/text "bar baz", :e/types #{:code}}]}]}])))

(deftest test-parse-2-unordered-lists-w-modified-text
  (is (= (parse (str "- foo *bar baz*\n"
                     "- foo __bar baz__\n\n"
                     "- foo -bar baz-\n"
                     "- foo `bar baz`\n\n"))
         [{:p/type :unordered-list, :p/lines [{:l/elements [{:e/text "foo ", :e/types #{}}
                                                                  {:e/text "bar baz", :e/types #{:em}}]}
                                              {:l/elements [{:e/text "foo ", :e/types #{}}
                                                            {:e/text "bar baz", :e/types #{:strong}}]}]}
          {:p/type :unordered-list, :p/lines [{:l/elements [{:e/text "foo ", :e/types #{}}
                                                            {:e/text "bar baz", :e/types #{:strike}}]}
                                              {:l/elements [{:e/text "foo ", :e/types #{}}
                                                                  {:e/text "bar baz", :e/types #{:code}}]}]}])))

(deftest test-parse-blockquote
  (is (= (parse (str "> foo bar baz\n"
                     "> foo bar baz\n\n"))
         [{:p/type :blockquote, :p/lines [{:l/elements [{:e/text "foo bar baz", :e/types #{}}]}
                                          {:l/elements [{:e/text "foo bar baz", :e/types #{}}]}]}])))

(deftest test-parse-code-block
  (is (= (parse (str "``` clojure\n"
                     "(+ 1 2)\n"
                     "(identity +)\n"
                     "```\n"))
          [{:p/type :code, :p/language "clojure"
                           :p/lines [{:l/elements [{:e/text "(+ 1 2)", :e/types #{}}]}
                                     {:l/elements [{:e/text "(identity +)", :e/types #{}}]}]}])))



;(deftest test-reduce-spans
;  (is (= (reduce-spans [])
;         []))
;  (is (= (reduce-spans [{:text "a", :e/types #{}}])
;         [{:text "a", :e/types #{}}]))
;  (is (= (reduce-spans [{:text "a", :e/types #{}} {:text "b", :e/types #{:em}}])
;         [{:text "a", :e/types #{}} {:text "b", :e/types #{:em}}]))
;  (is (= (reduce-spans [{:text "a", :e/types #{}} {:text "b", :e/types #{}}])
;         [{:text "ab", :e/types #{}}]))
;  (is (= (reduce-spans [{:text "a", :e/types #{}} {:text "b", :e/types #{}} {:text "a", :e/types #{}}])
;         [{:text "aba", :e/types #{}}]))
;  (is (= (reduce-spans [{:text "a", :e/types #{}} {:text "b", :e/types #{:em}} {:text "a", :e/types #{}}])
;         [{:text "a", :e/types #{}} {:text "b", :e/types #{:em}} {:text "a", :e/types #{}}])))
;

;(deftest test-split-text-into-slides
;  (is (= (split-text-into-slides (str "---\n" "comment1 line1\ncomment1 line2\n"
;                                      "===\n" "slide1 line1\nslide1 line2\n"
;                                      "---\n" "comment2 line1\ncomment2 line2\n"
;                                      "===\n" "slide2 line1\nslide2 line2\n"
;                                      "===\n" "slide3 line1\nslide3 line2\n"))
;         [{:s/type :comment :s/text "comment1 line1\ncomment1 line2\n"}
;          {:s/type :slide :s/text "slide1 line1\nslide1 line2\n"}
;          {:s/type :comment :s/text "comment2 line1\ncomment2 line2\n"}
;          {:s/type :slide :s/text "slide2 line1\nslide2 line2\n"}
;          {:s/type :slide :s/text "slide3 line1\nslide3 line2\n"}]))
;
;  (is (= (split-text-into-slides (str "===\n" "slide1 line1\nslide1 line2\n"
;                                      "---\n" "comment1 line1\ncomment1 line2\n"
;                                      "---\n" "comment2 line1\ncomment2 line2\n"
;                                      "===\n" "slide2 line1\nslide2 line2\n"
;                                      "===\n" "slide3 line1\nslide3 line2\n"))
;         [{:s/type :slide :s/text "slide1 line1\nslide1 line2\n"}
;          {:s/type :comment :s/text "comment1 line1\ncomment1 line2\n"}
;          {:s/type :comment :s/text "comment2 line1\ncomment2 line2\n"}
;          {:s/type :slide :s/text "slide2 line1\nslide2 line2\n"}
;          {:s/type :slide :s/text "slide3 line1\nslide3 line2\n"}]))
;
;  (is (= (split-text-into-slides (str      "slide1 line1\nslide1 line2\n"
;                                   "---\n" "comment1 line1\ncomment1 line2\n"
;                                   "---\n" "comment2 line1\ncomment2 line2\n"
;                                   "===\n" "slide2 line1\nslide2 line2\n"
;                                   "===\n" "slide3 line1\nslide3 line2\n"))
;        [{:s/type :slide :s/text "slide1 line1\nslide1 line2\n"}
;         {:s/type :comment :s/text "comment1 line1\ncomment1 line2\n"}
;         {:s/type :comment :s/text "comment2 line1\ncomment2 line2\n"}
;         {:s/type :slide :s/text "slide2 line1\nslide2 line2\n"}
;         {:s/type :slide :s/text "slide3 line1\nslide3 line2\n"}])))


(run-tests)
;(.log js/console (vec (parser "# foo + *bar*!\n\n")))
