(ns deckatron.parser
  (:require
    [instaparse.core :as insta]
    [cljs.test :refer-macros [deftest testing is run-tests]]))

(enable-console-print!)


(def ^:private SLIDES-SEPARATOR #"---\n")

(def ^:private parser
  ;;https://github.com/chameco/Hitman/blob/master/src/hitman/core.clj#L9
  (insta/parser
    "<Blocks> = (Paragraph | Header | List | Ordered | Code | Rule)+
    Header = Line Headerline Blankline+
    <Headerline> = (h1 | h2)
    h1 = '# '
    h2 = '## '
    List = Listline+ Blankline+
    Listline = Listmarker Whitespace+ Word (Whitespace Word)* EOL
    <Listmarker> = <'+' | '*' | '-'>
    Ordered = Orderedline+ Blankline+
    Orderedline = Orderedmarker Whitespace* Word (Whitespace Word)* EOL
    <Orderedmarker> = <#'[0-9]+\\.'>
    Code = Codeline+ Blankline+
    Codeline = <Space Space Space Space> (Whitespace | Word)* EOL
    Rule = Ruleline Blankline+
    <Ruleline> = <'+'+ | '*'+ | '-'+>
    Paragraph = Line+ Blankline+
    <Blankline> = Whitespace* EOL
    <Line> = Linepre Word (Whitespace Word)* Linepost EOL
    <Linepre> = (Space (Space (Space)? )? )?
    <Linepost> = Space?
    <Whitespace> = #'(\\ | \\t)+'
    <Space> = ' '
    <Word> = #'\\S+'
    <EOL> = <'\\n'>"))

(defn- ->element [s & types]
  {:text s :e/types (set types)})

(defn- ->paragraph [type elements]
  {:p/type type :elements elements})


(def ^:private SPAN-RULES
  ;; https://github.com/chameco/Hitman/blob/master/src/hitman/core.clj#L36
  ;; order matters!
  [[#"`(\S+)`"             (fn [s] (->element s :code))]
   [#"\*\*(\S+)\*\*"       (fn [s] (->element s :strong))]
   [#"__\*(\S+)\*__"       (fn [s] (->element s :em :strong))]
   [#"__(\S+)__"           (fn [s] (->element s :strong))]
   [#"\*(\S+)\*"           (fn [s] (->element s :em))]
   [#"_(\S+)_"             (fn [s] (->element s :em))]])


(defn- parse-header [block]
  "'hello world\n=\n\n'
  ==>
  [:Header \"hello\" \" \" \"world\" [:h1 \"=\"]]"
  (let [tag (-> block last first)
        txt (-> block rest drop-last)
        header {:e/types #{}
                :text (apply str txt)}]
    (->paragraph tag [header])))


(defn- parse-span [s]
  (let [parse-fn (fn [[regex f]]
                   (when-let [groups (re-matches regex s)]
                     (apply f (rest groups))))
        res (->> SPAN-RULES
                 (map parse-fn)
                 (remove nil?)
                 first)]
    (if (nil? res)
      (->element s)
      res)))

(defn- reduce-spans [ss]
  (let [f (fn [m1 m2]
            (update m1 :text str (:text m2)))
        by-types (partition-by :e/types ss)]
    (mapv #(reduce f %) by-types)))

(defn- parse-list-line [line]
  (->> (rest line)
       (map parse-span)
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


(defn- parse-block [b]
  (case (first b)
    :heading (parse-header b)
    :unordered-list (parse-list b)
    :ordered-list (parse-ordered-list b)))


(defn parse [s]
  (->> s parser (mapv parse-block)))


(defn split-text-into-slides [t]
  (remove clojure.string/blank? (clojure.string/split t SLIDES-SEPARATOR)))


;; TESTS

;(deftest test-parse-span
;  (is (= (parse-span "__strong__")
;         {:e/types #{:strong} :text "strong"}))
;  (is (= (parse-span "__str ong__")
;         {:e/types #{:strong} :text "str ong"}))
;  (is (= (parse-span "__*stronganditalic*__")
;         {:e/types #{:em :strong} :text "stronganditalic"})))



;(deftest test-grammar
;  (is (= (vec (parser "# hello + world\n\n"))
;         [[:heading "#" "hello world"]]))
;  (is (= (vec (parser "# hello world\n\n"))
;         [[:heading "#" "hello world"]]))
;  (is (= (vec (parser "## hello world\n\n"))
;         [[:heading "##" "hello world"]]))
;  (is (= (vec (parser "- first line\n- second line\n\n"))
;         [[:unordered-list
;           [:unordered-item "first line"]
;           [:unordered-item "second line"]]]))
;  (is (= (vec (parser "1. first line\n2. second line\n\n"))
;         [[:ordered-list
;           [:ordered-item "first line"]
;           [:ordered-item "second line"]]]))
;  (is (= (vec (parser "``` clojure\n(+ 1 2)\n```\n\n"))
;         [[:pre-code [:lang "clojure"] [:codetext "(+ 1 2)"]]]))
;  (is (= (vec (parser "```\n(+ 1 2)\n```\n\n"))
;         [[:pre-code [:codetext "(+ 1 2)"]]])))
;
;
;(deftest test-parse-block
;  (is (= (parse-block [:heading "#" "hello + world!"])
;         {:p/type :h1, :p/lines [{:l/elements [{:text "hello world", :e/types #{}}]}]}))
;  (is (= (parse-block [:heading "##" "hello world"])
;         {:p/type :h2, :p/lines [{:l/elements [{:text "hello world", :e/types #{}}]}]}))
;  (is (= (parse-block [:heading "#" "hello world"])
;         {:p/type :h1, :p/lines [{:text "hello ", :e/types #{}}
;                                  {:text "world", :e/types #{:em}}]})))



(deftest test-parse-heading-1
  (is (= (parse "# foo + *bar*!\n\n")
         [{:p/type :h1, :p/lines [{:l/elements [{:e/text "foo + *bar*!", :e/types #{}}]}]}])))

(deftest test-parse-heading-2
  (is (= (parse "## foo + bar baz_?\n\n")
         [{:p/type :h2, :p/lines [{:l/elements [{:e/text "foo + bar baz_?", :e/types #{}}]}]}])))

(deftest test-parse-block-w-plain-text
  (is (= (parse "foo *bar!\n")
         [{:p/type :text, :p/lines [{:l/elements [{:e/text "foo *bar!", :e/types #{}}]}]}])))

(deftest test-parse-block-w-modified-text
  (is (= (parse "foo *bar baz* _qux_\n")
         [{:p/type :text, :p/lines [{:l/elements [{:e/text "foo ", :e/types #{}}
                                                  {:e/text "bar baz qux", :e/types #{:em}}]}]}])))

(deftest test-parse-block-w-nested-modified-text
  (is (= (parse "foo __*bar baz*__ _qux_.\n")
         [{:p/type :text, :p/lines [{:l/elements [{:e/text "foo ", :e/types #{}}
                                                  {:e/text "bar baz", :e/types #{:em :strong}}
                                                  {:e/text "qux", :e/types #{:em}}
                                                  {:e/text ".", :e/types #{}}]}]}])))
(deftest test-parse-2-ordered-lists-w-modified-text
    (is (= (parse (str "1. foo *bar baz*\n"
                       "2. foo __bar baz__\n\n"
                       "1. foo -bar baz-\n"
                       "2. foo `bar baz`\n\n"))
           [{:p/type :ordered-list, :p/lines [{:l/elements [{:e/text "foo ", :e/types #{}}
                                                            {:e/text "bar baz", :e/types #{:em}}]}
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

(deftest test-parse-code-block
  (is (= (parse (str "``` clojure\n"
                     "(+ 1 2)\n"
                     "(identity +)\n"
                     "```\n"))
          [{:p/type :code, :p/lines [{:l/elements [{:e/text "(+ 1 2)", :e/types #{}}]}
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

;(deftest test-parse
;  ;; list:
;  (is (= (parse "first line\n=\n\n")
;         [{:p/type :h1, :elements [{:text "first line", :e/types #{}}]}]))
;  ;; list:
;  (is (= (parse "* first line\n* second line\n\n")
;         [{:p/type :list, :elements [[{:text " first line", :e/types #{}}]
;                                     [{:text " second line", :e/types #{}}]]}]))
;  ;; list line with inline md:
;  (is (= (parse "* line *with* emph\n\n")
;         [{:p/type :list, :elements [[{:text " line ", :e/types #{}}
;                                      {:text "with", :e/types #{:em}}
;                                      {:text " emph", :e/types #{}}]]}]))
;  ;; ordered list:
;  (is (= (parse "1. first line\n2. second line\n\n")
;         [{:p/type :ordered-list, :elements [[{:text "first line", :e/types #{}}]
;                                             [{:text "second line", :e/types #{}}]]}]))
;  )

(deftest test-split-text-into-slides
  (is (= (split-text-into-slides "---\nslide1line1\nlide1line2\n---\nslide2line1\n")
         '("slide1line1\nlide1line2\n" "slide2line1\n"))))

(run-tests)