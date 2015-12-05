(ns deckatron.parser
  (:require
    [instaparse.core :as insta]))


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

(defn- ->element
  [s & types]
  {:text s :e/types (set types)})

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
