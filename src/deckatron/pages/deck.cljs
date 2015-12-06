(ns deckatron.pages.deck
  (:require-macros
    [cljs.core.async.macros :refer [go]])
  (:require
    [clojure.string :as str]
    [rum.core :as rum]
    [deckatron.core :as core]
    [deckatron.util :as u]
    [cljs.core.async :refer [put! chan <! >! timeout]]
    [deckatron.parser :as parser]
    [deckatron.pages.edit :as edit]
    [deckatron.pages.read :as read]
    [deckatron.pages.present :as present]))


(enable-console-print!)


(defonce socket nil)


(defonce *pending-content (atom nil))


;; latest deck with applied/confirmed changes from server
(defonce *server-deck (atom nil))  

;; = server-deck + pending local changes
;; Client pages work with this one, chaning it directly.
;; Changes made to this atom will be automatically synced to server
(defonce *pending-deck (atom nil))


(def sync-interval 1000)


(defn send! [message]
  (when (and socket (== 1 (.-readyState socket))) ;; WS_OPEN
    (.send socket (u/obj->transit message))))


;; Once change has been made to *pending-deck, we schedule
;; to send it to server in 1000ms
(add-watch *pending-deck ::sync
  (fn [_ _ _ new]
    (when (not= new @*server-deck)
;;       (println "Got delta" (u/diff @*server-deck new))
      (js/setTimeout
        (fn []
          ;; When it’s time to sync, we check that we still
          ;; have some unsent changes 
          (let [old @*server-deck
                new @*pending-deck]
            (when (not= old new)
              (println "Sending delta" (u/diff old new))
              (send! { :deck/id (:deck/id old)
                       :patch   (u/diff old new) }))
            ;; after we sent patch, we apply it locally
            (reset! *server-deck new)
            (assert (= @*server-deck @*pending-deck))))
       sync-interval))))


(defn can-spectate? [deck]
  (and (:presenter-slide deck)
       (contains? (:deck/spectators deck) (:user/id deck))))


(rum/defc menu-mode [deck-id text mode]
  [:.menu-mode
    (merge { :class (when (= text mode) "menu-mode_selected") }
           (core/turbolink (str "/deck/" deck-id "/" text) true))
    text])


(rum/defc menu [deck mode]
  (let [deck-id (:deck/id deck)
        author? (core/author? deck)
        spectators (count (disj (:deck/spectators deck) core/user-id))]
    [:table.menu
      [:tbody
        [:tr
          [:td.td-logo
            [:a.logo (core/turbolink "/") [:div "⟵"]]]
          [:td.td-modes
            [:.menu-modes
              (when author?
                (menu-mode deck-id "Edit" mode))
              (menu-mode deck-id "Read" mode)
              (if author?
                (menu-mode deck-id "Present" mode)
                (when (can-spectate? deck)
                  (menu-mode deck-id "Spectate" mode)))]]
          [:td.td-theme
            (when author?
              [:.menu-theme [:div "Theme" [:span {:style {"float" "right"}} "▾"]]])]
          [:td.td-stats
            [:.menu-stats
                (if (pos? spectators)
                  [:div
                    { :title (pr-str (:deck/spectators deck)) }
                    [:.menu-stats-bullet.menu-stats-bullet_live]
                    (str spectators " watching live")]
                  [:div
                    (str (count (:deck/viewed-by deck)) " total views")])]]]]]))


(rum/defc deck-page < rum/reactive [mode]
  (when-let [deck (rum/react *pending-deck)]
    [:.page_deck
      (menu deck mode)
      (case mode
        "Edit"     (edit/edit-page *pending-deck)
        "Read"     (read/read-page *pending-deck)
        "Present"  (present/present-page *pending-deck)
        "Spectate" (present/spectate-page *pending-deck))]))


;; When patch comes from server
(defn on-server-push [patch]
  (println "Server push:" patch)
  ;; Calculating what changes have we accumulated up to this point
  (let [delta (u/diff @*server-deck @*pending-deck)]
    ;; We apply it to confirmed deck first
    (swap! *server-deck u/patch patch)
;;     (println "New state" @*server-deck)
    ;; Take new server-deck and apply local changes to it
    (reset! *pending-deck (u/patch @*server-deck delta))))


(defn validate-mode! [deck mode]
  (when deck
    (let [author? (core/author? deck)
          switch! #(core/switch! (str "/deck/" (:deck/id deck) "/" %))]
      (case [author? mode]
        [true  nil]        (switch! "Edit")
        [true  "Spectate"] (switch! "Present")
        [false nil]        (switch! "Read")
        [false "Present"]  (switch! "Spectate")
        [false "Edit"]     (switch! "Read")
        [false "Spectate"] (when-not (can-spectate? deck)
                             (switch! "Read"))
        nil))))


(defmethod core/start-page! :deck [[_ deck-id mode] mount-el]
  ;; TODO watch websocket status, reconnect
  (when (nil? socket)
    (println "Starting :deck page" deck-id)
    (set! socket
      (doto (js/WebSocket. (str "ws://" js/location.host "/api/deck/" deck-id))
        (aset "onmessage"
          (fn [payload]
            (-> (.-data payload)
                (u/transit->obj )
                :patch
                (on-server-push)))))))

  (add-watch *pending-deck ::validate-mode
    (fn [_ _ _ deck]
      (validate-mode! deck mode)))

  (rum/mount (deck-page mode) mount-el))


(defmethod core/stop-page! :deck [[_ deck-id] [next-mode next-deck-id]]
  (let [compatible? (and (= next-mode :deck)
                         (= next-deck-id deck-id))]
    (when-not compatible?
      (println "Stopping :deck page" deck-id)
      (when socket
        (.close socket)
        (set! socket nil))
      (remove-watch *pending-deck ::validate-mode)
      (reset! *server-deck nil)
      (reset! *pending-deck nil))))


(def TEST-CONNECTION-INTERVAL 5000)

(defn socket-closed? [s]
  (if-not s
    true
    (= 3 (.-readyState socket))))

#_(go
  (while true
    (<! (timeout TEST-CONNECTION-INTERVAL))
    (if (socket-closed? socket)
      (do
        (println (str "connection died, reconnecting with deck-id: " @*deck-id))
        (refresh! @*deck-id))
      (println (str "connection is alive, deck-id: " @*deck-id ", socket status: " (.-readyState socket))))))
