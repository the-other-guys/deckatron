(ns deckatron.pages.layouts
  (:require
    [rum.core :as rum]
    [clojure.string :as str]))

(rum/defc default-layout [slide]
  [:.slide
   [:.slide-inner
    [:.slide-text (str/trim (:s/text slide))]]])

(rum/defc head-only-layout [slide]
  (let [e (-> slide :s/paragraphs first :p/lines first :l/elements first)]
    [:.slide
     [:.slide-inner
      [:.slide-text (str "I am head-only layout:\n" (:e/text e))]]]))


(def LAYOUTS
  {[:h1] head-only-layout
   [:h2] head-only-layout
   [:h3] head-only-layout
   [:h4] head-only-layout})


(defn slide->layout [slide]
  (print (str "slide: " slide))
  (let [key    (mapv :p/type (:s/paragraphs slide))
        layout (get LAYOUTS key default-layout)]
    (print (str "layout key: " key))
    (layout slide)))