(ns deckatron.app
  (:require
    [rum.core :as rum]
    [cognitect.transit :as t]
    [deckatron.parser :as parser]))


(enable-console-print!)


(defn read-transit-str [s]
  (t/read (t/reader :json) s))


(defn write-transit-str [o]
  (t/write (t/writer :json ) o))


(defonce *state (atom { :count 0
                        :message "Hello, world!" }))

(declare send!)


(defonce socket
  (doto (js/WebSocket. (str "ws://" js/location.host "/api/websocket"))
    (aset "onmessage"
      (fn [payload]
        (let [message (read-transit-str (.-data payload))]
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
    (.send socket (write-transit-str message))))


(rum/defc app < rum/reactive []
  (let [state (rum/react *state)]
    [:div "[" (:count state) "] " (pr-str (:message state)) ]))


(defn ^:export refresh []
  (send! "refreshed")
  (rum/mount (app) (js/document.querySelector "#app")))
