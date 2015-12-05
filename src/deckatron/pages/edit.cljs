(ns deckatron.pages.edit
  (:require
    [clojure.string :as str]
    [rum.core :as rum]
    [deckatron.core :as core]
    [deckatron.util :as u]
    [deckatron.parser :as parser]))


(enable-console-print!)


(defonce *pending-content (atom nil))


(defn send! [socket message]
  (when (== 1 (.-readyState socket)) ;; WS_OPEN
    (.send socket (u/obj->transit message))))


(defn schedule-send! [*deck socket content]
  (when (nil? @*pending-content)
    (js/setTimeout
      (fn []
        (let [content @*pending-content ;; latest value ATM
              deck    @*deck            ;; -- // --
              patch   [{:deck/content (:deck/content deck)}
                       {:deck/content content}]]
          ;; TODO check that message was actually sent
          (send! socket
                 { :deck/id (:deck/id deck)
                   :patch   patch })
          (swap! *deck u/patch patch)
          (reset! *pending-content nil)))
      1000))
  (reset! *pending-content content))


(rum/defc edit-page < rum/reactive [*deck socket]
  (let [deck    (rum/react *deck)
        content (or (rum/react *pending-content)
                    (:deck/content deck))
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
                       (schedule-send! *deck socket (.. e -target -value))) }]
                                                  
      
      [:.slides
        { :style { :width  (str width "px")
                   :height (str height "px")
                   :font-size (str (u/width->font-size width) "px") } }
        (for [slide (core/slides content)]
          (core/slide slide))]]))
