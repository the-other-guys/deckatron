(ns deckatron.pages.home
  (:require
    [rum.core :as rum]
    [deckatron.util :as u]
    [deckatron.parser :as parser]))


(enable-console-print!)


(defonce socket nil)


(defonce *decks (atom {}))


(add-watch *decks ::log (fn [_ _ _ v] (println "Decks:" v)))


(rum/defc page < rum/reactive []
  (let [decks (rum/react *decks)]
    [:.decks
      (for [[deck-id deck] (sort-by :deck/id decks)]
        [:.deck
          [:a {:href (str "/deck/" deck-id)} deck-id]])
      [:.deck
        [:a {:href "/create-deck"} "+ Create new deck"]]]))


(defn refresh! []
  (when socket
    (.close socket)
    (reset! *decks {}))
  
  (set! socket
    (doto (js/WebSocket. (str "ws://" js/location.host "/api/decks"))
      (aset "onmessage"
        (fn [payload]
          (let [data (u/transit->obj (.-data payload))]
            (println "Received:" data)
            (swap! *decks update (:deck/id data) u/patch (:patch data)))))))
  
  (rum/mount (page) (js/document.getElementById "app")))


