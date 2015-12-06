(ns deckatron.pages.layouts
  (:require
    [rum.core :as rum]
    [clojure.string :as str]))

(rum/defc default-layout [slide]
  [:.slide
   [:.slide-inner
    [:.slide-text (str/trim (:s/text slide))]]])

(rum/defc head-only-layout [slide]
  (let [t (-> slide :s/paragraphs first :p/type)
        e (-> slide :s/paragraphs first :p/lines first :l/elements first)]
    [:.slide
     [:.slide-inner
      [:.slide-text
       [tag (str "I am head-only layout:\n" (:e/text e))]]]]))


(rum/defc two-headed-centered-layout [slide]
  (let [t1 (-> slide :s/paragraphs first :p/type)
        t2 (-> slide :s/paragraphs second :p/type)
        e1 (-> slide :s/paragraphs first :p/lines first :l/elements first)
        e2 (-> slide :s/paragraphs second :p/lines first :l/elements first)]
    [:.slide
     [:.slide-inner
      [:.slide-text
       [t1 (str "I am two-headed layout:\n" (:e/text e1))]
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
  (print (str "slide: " slide))
  (let [key    (mapv :p/type (:s/paragraphs slide))
        layout (get LAYOUTS key default-layout)]
    (print (str "layout key: " key))
    (layout slide)))