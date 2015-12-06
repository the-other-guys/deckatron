(ns deckatron.pages.layouts
  (:require
    [rum.core :as rum]
    [clojure.string :as str]))

(rum/defc default-layout [slide]
    [:.slide
     [:.slide-inner
      [:.slide-text (str/join slide)]]])

(rum/defc head-only-layout [slide]
  (let [t (-> slide first :p/type)
        e (-> slide first :p/lines first :l/elements first :e/text)]
    [:.slide
     [:.slide-inner
      [:.slide-text
       [t e]]]]))


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
