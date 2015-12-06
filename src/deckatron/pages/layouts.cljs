(ns deckatron.pages.layouts
  (:require
    [rum.core :as rum]
    [deckatron.util :as u]
    [clojure.string :as str]))

(defn- get-tag [props]
  (if (contains? props :link) :a
    (if (contains? props :strong) :strong
      (if (contains? props :em) :em
       (if (contains? props :image) :img
         :span)))))

(defn- get-props [tg]
  (let [props (:e/types tg)]
       (if (contains? props :link) {:href (:e/href tg)}
         (if (contains? props :image) {:src (:e/href tg) :alt (:e/text tg) :width "150em"} {}))))

(defn- get-text [tg]
  (let [props (:e/types tg)]
       (if (contains? props :image) nil (:e/text tg))))

(defn- parse-span [sp]
  (let [ems (:l/elements sp)]
      (map (fn [%] [(get-tag (:e/types %)) (get-props %) (get-text %)]) ems)))

(defn- parse-tx [lines]
  (map parse-span lines))

(defn- parse-li [lines]
  (map (fn [%] [:li (parse-span %)]) lines))

(defn- parse-blockquote [lines]
  (map (fn [%] [:blockquote (parse-span %)]) lines))

(defn- parse-pre [lines]
  (str/join "\n" (map (fn [l] (str/join (map :e/text (:l/elements l)))) lines)))

(defn- parse-p [p]
  (case (:p/type p)
    :h1 [:h1 (parse-tx (:p/lines p))]
    :h2 [:h2 (parse-tx (:p/lines p))]
    :h3 [:h3 (parse-tx (:p/lines p))]
    :h4 [:h4 (parse-tx (:p/lines p))]
    :ordered-list [:ol (parse-li (:p/lines p))]
    :unordered-list [:ul (parse-li (:p/lines p))]
    :blockquote (parse-blockquote (:p/lines p))
    :code [:pre (parse-pre (:p/lines p))]
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
       (->html slide)]]]))


(rum/defc two-headed-centered-layout [slide]
  (let [t1 (-> slide first :p/type)
        t2 (-> slide second :p/type)
        e1 (-> slide first :p/lines first :l/elements first)
        e2 (-> slide second :p/lines first :l/elements first)]
    [:.slide
     [:.slide-inner
      [:.slide-text
       (->html slide)]]]))


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
   (print (str "slide: " slide))
  (let [ast    (:s/paragraphs slide)
        key    (mapv :p/type ast)
        layout (get LAYOUTS key default-layout)]
     (print (str "layout key: " key))
    (layout ast)))
