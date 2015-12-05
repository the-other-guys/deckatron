(ns deckatron.app
  (:require
    [rum.core :as rum]
    [deckatron.util :as u]
    [deckatron.parser :as parser]))


(enable-console-print!)

(defonce *state (atom { :count 0
                        :message "Hello, world!" }))

(declare send!)


(defonce socket
  (doto (js/WebSocket. (str "ws://" js/location.host "/api/websocket"))
    (aset "onmessage"
      (fn [payload]
        (let [message (u/transit->obj (.-data payload))]
          (swap! *state #(-> %
                           (update :count inc)
                           (assoc :message message))))))
    (aset "onopen"
      (fn []
        (send! {:op :deck/new})
        ;(send! {:op :deck/save :id :TODO-fetch-from-response :content (str (new js/Date))})
        (send! {:op :deck/list-mine})))))


(defn send! [message]
  (when (== 1 (.-readyState socket)) ;; WS_OPEN
    (.send socket (u/obj->transit message))))


(rum/defc app < rum/reactive []
  (let [state (rum/react *state)]
    [:div "[" (:count state) "] " (pr-str (:message state)) ]))


(defn ^:export refresh []
  (send! "refreshed")
  (rum/mount (app) (js/document.querySelector "#app")))
