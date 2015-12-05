(ns deckatron.pages.deck
  (:require
    [rum.core :as rum]
    [deckatron.util :as u]
    [deckatron.parser :as parser]))


(enable-console-print!)


(defonce socket nil)


(defonce *deck (atom nil))


(add-watch *deck ::log (fn [_ _ _ v] (println "Deck:" v)))


(rum/defc page < rum/reactive []
  [:.deck
    (for [[k v] (rum/react *deck)]
      [:div (str k ": " v)])])


(defn refresh! [deck-id]
  (when socket
    (.close socket)
    (reset! *deck nil))
  
  (set! socket
    (doto (js/WebSocket. (str "ws://" js/location.host "/api/deck/" deck-id))
      (aset "onmessage"
        (fn [payload]
          (let [data (u/transit->obj (.-data payload))]
            (println "Received:" data)
            (swap! *deck u/patch (:patch data)))))))
  
  (rum/mount (page) (js/document.getElementById "app")))


(defn send! [message]
  (when (== 1 (.-readyState socket)) ;; WS_OPEN
    (.send socket (u/obj->transit message))))

