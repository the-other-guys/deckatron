(ns deckatron.pages.home
  (:require
    [rum.core :as rum]
    [deckatron.util :as u]
    [deckatron.core :as core]))


(enable-console-print!)


(defonce socket nil)


(defonce *decks (atom {})) ;; deck-id => Deck

;; (add-watch *decks ::log (fn [_ _ _ v] (println "Decks:" v)))


(rum/defc deck [deck]
  [:.deck
    [:.deck-slide
      { :class [ (str "deck-slide_" (:deck/id deck))
                 (:deck/theme deck "default") ] }
      [:a (core/turbolink (str "/deck/" (:deck/id deck) (when (core/presenting? deck) "/Spectate")))
        (core/slide (-> deck :deck/content core/->first-slide-only))]
      #_[:.deck-shine]]
    (let [spectators (count (:deck/spectators deck))]
      (if (core/presenting? deck)
        [:.deck-subtext.deck-subtext_spectators spectators " watching now"]
        [:.deck-subtext (count (:deck/viewed-by deck)) " viewers"]))])


(rum/defc decks-list [decks & [additional]]
  [:.decks-list
    additional
    (for [d (->> (sort-by :deck/id decks) reverse)]
      (deck d))])


(rum/defc page < rum/reactive []
  (let [decks (vals (rum/react *decks))
        [live intro yours rest] (u/split decks
                                  core/presenting?
                                  #(= "user-deckatron" (:user/id %))
                                  #(= core/user-id (:user/id %)))]
    [:.page_home
      [:.layer.layer_title
        [:.layer-inner
          [:.title
            (map #(vector :span %) "DECK–a–TRON")]
          [:.subtitle "Slides, presentations, notes"]]]
     
      [:.layer
        [:.layer-inner
          (decks-list intro)]]
     
      (when (not-empty live)
        [:.layer.layer_live
          [:.layer-inner
            [:h1 [:.text "L I V E"]]
            (decks-list live)]])
     
      [:.layer.authors-decks
        [:.layer-inner
          [:h1 [:.btngroup [:.btn "Your decks"]]]
          (decks-list yours
            [:.deck.deck_new
              [:.deck-slide
                [:a.slide {:href "/create-deck"}
                  [:.slide-inner
                    [:.slide-text "+ Create new deck"]]]]])]]
     
      [:.layer
        [:.layer-inner
          [:h1 [:.btngroup [:.btn "Other people’s decks"]]]
          (decks-list rest)]]]))


(defmethod core/start-page! :home [_ mount-el]
  ;; TODO watch websocket status, reconnect
  (when (nil? socket)
    (println "Starting :home page")
    (set! socket
      (doto (js/WebSocket. (str "ws://" js/location.host "/api/decks"))
        (aset "onmessage"
          (fn [payload]
            (let [data (u/transit->obj (.-data payload))]
;;               (println "Received:" data)
              (swap! *decks update (:deck/id data) u/patch (:patch data))))))))
  
  (rum/mount (page) mount-el))


(defmethod core/stop-page! :home [_ next-path]
  (let [compatible? (= (first next-path) :home)]
    (when-not compatible?
      (println "Stopping :home page")
      (when socket
        (.close socket)
        (set! socket nil))
      (reset! *decks {}))))
