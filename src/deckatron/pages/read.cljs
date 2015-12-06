(ns deckatron.pages.read
  (:require
    [clojure.string :as str]
    [rum.core :as rum]
    [deckatron.core :as core]
    [deckatron.util :as u]
    [deckatron.parser :as parser]))


(enable-console-print!)


(rum/defc read-page < rum/reactive [*deck]
  (let [deck  (rum/react *deck)
        width (min (-> (rum/react core/*window-width) (/ 1.5))
                   600)]
    [:.page_deck-read
      [:.slides
        { :class (:deck/theme deck "default")
          :style { :width  (str width "px")
                   :font-size (str (u/width->font-size width) "px") } }
        (for [slide (core/->slides-and-notes (:deck/content deck))]
          (core/slide slide))
       
        (when-let [id (:deck/forked-from deck)]
          [:.note { :style { "text-align" "center" } }
            [:h2 "Forked from " [:a (core/turbolink (str "/deck/" id)) "this deck"]]])]]))

