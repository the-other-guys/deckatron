(ns deckatron.pages.deck
  (:require
    [clojure.string :as str]
    [rum.core :as rum]
    [deckatron.util :as u]
    [deckatron.parser :as parser]))


(enable-console-print!)


(defonce socket nil)


(defonce *deck (atom nil))


(defonce *pending-content (atom nil))


(defn send! [message]
  (when (== 1 (.-readyState socket)) ;; WS_OPEN
    (.send socket (u/obj->transit message))))


;; TODO add local storage persistence
(add-watch *pending-content ::send
  (fn [_ _ old new]
    (when (nil? old)
      (js/setTimeout
        (fn []
          (let [content @*pending-content
                deck    @*deck]
            ;; TODO check that message was actually sent
            (send! { :deck/id (:deck/id deck)
                     :patch   [{:deck/content (:deck/content deck)}
                               {:deck/content content}] })
            (swap! *deck assoc :deck/content content)
            (reset! *pending-content nil)))
       1000))))


(rum/defc page < rum/reactive []
  (let [deck  (rum/react *deck)
        value (or (rum/react *pending-content)
                  (:deck/content deck))]
    [:.page_deck
      [:textarea.editor 
        { :value     value
          :on-change (fn [e]
                       (reset! *pending-content (.. e -target -value))) }]
      [:.slides
        (for [slide (str/split value #"(?:---|===)")]
          [:.slide
            [:.slide-inner
              [:.slide-text slide]]])]]))


(defn refresh! [deck-id]
  (when socket
    (.close socket)
    (reset! *deck nil))
  
  (println "Loading deck" deck-id)
  ;; TODO watch websocket status, reconnect
  (set! socket
    (doto (js/WebSocket. (str "ws://" js/location.host "/api/deck/" deck-id))
      (aset "onmessage"
        (fn [payload]
          (let [data (u/transit->obj (.-data payload))]
            (println "Received:" data)
            (swap! *deck u/patch (:patch data)))))))
  
  (rum/mount (page) (js/document.getElementById "app")))



