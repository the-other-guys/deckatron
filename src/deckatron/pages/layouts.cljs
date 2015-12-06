(ns deckatron.pages.layouts
  (:require
    [rum.core :as rum]
    [clojure.string :as str]))

(defn- get-tag [props]
  :span)

(defn- parse-span [sp]
  (let [ems (:l/elements sp)]
      (mapv (fn [%] [(get-tag (:e/types %)) (:e/text %)]) ems)))

(defn- parse-tx [lines]
  (mapcat parse-span lines))

(defn- parse-li [lines]
  (map (fn [%] [:li (vec (flatten (parse-span %)))]) lines))

(defn- parse-p [p]
  (case (:p/type p)
    :h1 [:h1 (parse-tx (:p/lines p))]
    :h2 [:h2 (parse-tx (:p/lines p))]
    :h3 [:h3 (parse-tx (:p/lines p))]
    :h4 [:h4 (parse-tx (:p/lines p))]
    :ordered-list [:ol (parse-li (:p/lines p))]
    :unordered-list [:ul (parse-li (:p/lines p))]
    [:div (parse-tx (:p/lines p))]
    )
  )

(defn- ->html [slide]
  (map parse-p slide))

(rum/defc default-layout [slide]
    [:.slide
     [:.slide-inner
      [:.slide-text (->html slide)]]])

; e (-> slide first :p/lines first :l/elements first :e/text)
(rum/defc head-only-layout [slide]
  (let [t (-> slide first :p/type)]
    [:.slide
     [:.slide-inner
      [:.slide-text
       [t (->html slide)]]]]))


(rum/defc two-headed-centered-layout [slide]
  (let [t1 (-> slide first :p/type)
        t2 (-> slide second :p/type)
        e1 (-> slide first :p/lines first :l/elements first)
        e2 (-> slide second :p/lines first :l/elements first)]
    [:.slide
     [:.slide-inner
      [:.slide-text
       [t1 (str "2H: \n" (:e/text e1))]
       [t2 (:e/text e2)]]]]))


(def LAYOUTS
  {[:h1] head-only-layout
   [:h2] head-only-layout
   [:h3] head-only-layout
   [:h4] head-only-layout
   [:h1 :h2] two-headed-centered-layout
   [:h1 :h3] two-headed-centered-layout
   [:h1 :h4] two-headed-centered-layout
   [:h2 :h1] two-headed-centered-layout
   [:h2 :h3] two-headed-centered-layout
   [:h2 :h4] two-headed-centered-layout})


(defn slide->layout [slide]
;   (print (str "slide: " slide))
  (let [key    (mapv :p/type slide)
        layout (get LAYOUTS key default-layout)]
;     (print (str "layout key: " layout))
    (layout slide)))
