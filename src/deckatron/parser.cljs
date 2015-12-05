(ns deckatron.parser
  (:require
    [instaparse.core :as insta]
    [cljs.test :refer-macros [deftest testing is run-tests]]))

(enable-console-print!)

(def ^:private parser
  ;;https://github.com/chameco/Hitman/blob/master/src/hitman/core.clj#L9
  (insta/parser
    "<Blocks> = (Paragraph | Header | List | Ordered | Code | Rule)+
    Header = Line Headerline Blankline+
    <Headerline> = h1 | h2
    h1 = '='+
    h2 = '-'+
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
  ;;https://github.com/chameco/Hitman/blob/master/src/hitman/core.clj#L36
  [[#"`(\S+)`"             (fn [s] (->element s :code))]
   [#"\*\*(\S+)\*\*"       (fn [s] (->element s :strong))]
   [#"__(\S+)__"           (fn [s] (->element s :strong))]
   [#"\*(\S+)\*"           (fn [s] (->element s :em))]
   [#"_(\S+)_"             (fn [s] (->element s :em))]])


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

(defn- parse-header [block]
  "'hello world\n=\n\n'
  ==>
  [:Header \"hello\" \" \" \"world\" [:h1 \"=\"]]"
  (let [tag (-> block last first)
        txt (-> block rest drop-last)
        header {:e/types #{tag}
                :text (apply str txt)}]
    (->paragraph :text [header])))


(defn- parse-block [b]
  (case (first b)
    :Header (parse-header b)
    :List (parse-list b)))


(defn parse [s]
  (->> s parser (mapv parse-block)))


(parse "* one one\n* two two\n\n")
[{:p/type :list, :elements [[{:text " one one", :e/types #{}}] [{:text " two two", :e/types #{}}]]}]


(deftest test-grammar
  (is (= (vec (parser "hello world\n=\n\n"))
         [[:Header "hello" " " "world" [:h1 "="]]]))
  (is (= (vec (parser "hello world\n-\n\n"))
         [[:Header "hello" " " "world" [:h2 "-"]]]))
  (is (= (vec (parser "* first line\n* second line\n\n"))
         [[:List [:Listline " " "first" " " "line"] [:Listline " " "second" " " "line"]]])))


(deftest test-reduce-spans
  (is (= (reduce-spans [])
         []))
  (is (= (reduce-spans [{:text "a", :e/types #{}}])
         [{:text "a", :e/types #{}}]))
  (is (= (reduce-spans [{:text "a", :e/types #{}} {:text "b", :e/types #{:em}}])
         [{:text "a", :e/types #{}} {:text "b", :e/types #{:em}}]))
  (is (= (reduce-spans [{:text "a", :e/types #{}} {:text "b", :e/types #{}}])
         [{:text "ab", :e/types #{}}]))
  (is (= (reduce-spans [{:text "a", :e/types #{}} {:text "b", :e/types #{}} {:text "a", :e/types #{}}])
         [{:text "aba", :e/types #{}}]))
  (is (= (reduce-spans [{:text "a", :e/types #{}} {:text "b", :e/types #{:em}} {:text "a", :e/types #{}}])
         [{:text "a", :e/types #{}} {:text "b", :e/types #{:em}} {:text "a", :e/types #{}}])))



(deftest test-parse
  ;; list:
  (is (= (parse "* first line\n* second line\n\n")
         [{:p/type :list, :elements [[{:text " first line", :e/types #{}}]
                                     [{:text " second line", :e/types #{}}]]}]))
  ;; list line with inline md:
  (is (= (parse "* line *with* emph\n\n")
         [{:p/type :list, :elements [[{:text " line ", :e/types #{}}
                                      {:text "with", :e/types #{:em}}
                                      {:text " emph", :e/types #{}}]]}]))
  )


(run-tests)