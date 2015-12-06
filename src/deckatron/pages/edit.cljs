(ns deckatron.pages.edit
  (:require
    [clojure.string :as str]
    [rum.core :as rum]
    [deckatron.core :as core]
    [deckatron.util :as u]
    [deckatron.parser :as parser]))


(enable-console-print!)

(rum/defc edit-page < rum/reactive [*deck]
  (let [deck    (rum/react *deck)
        content (:deck/content deck)
        width   (-> (rum/react core/*window-width) (/ 2))
        height  (rum/react core/*window-height) ]
    [:.page_deck-edit
      
      #_[:div.hidden-editor
        { :style { :width (str width "px") } }
        (str content " ")]
     
      [:textarea.editor
        { :style     { :width  (str width "px")
                       :height (str height "px") }
          :value     content
          :on-change (fn [e]
                       (swap! *deck assoc :deck/content (.. e -target -value))) }]
                                                  
      
      [:.slides
        { :style { :width  (str width "px")
                   :height (str height "px")
                   :font-size (str (u/width->font-size width) "px") } }
        (for [slide (core/slides content)]
          (core/slide slide))]]))
