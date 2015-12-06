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
      (println "Got delta" (u/diff @*server-deck new))
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


(defonce *mode (atom "Read"))


(add-watch *pending-deck ::default-mode
  (fn [_ _ old deck]
    (when (nil? old) ;; first load
      (if (= core/user-id (:user/id deck))
        (reset! *mode "Edit")
        (reset! *mode "Read")))
    (remove-watch *pending-deck ::default-mode)))


(rum/defc menu-mode [text mode]
  (let [selected? (= text mode)]
    [:.menu-mode
      { :class    (when selected? "menu-mode_selected")
        :on-click (fn [_] (reset! *mode text)) }
      text]))


(rum/defc menu [deck mode]
  (let [author? (= core/user-id (:user/id deck))
        spectators (count (:deck/spectators deck))]
    [:table.menu
      [:tbody
        [:tr
          [:td.td-logo
            [:a.logo { :href "/" } [:div "⟵"]]]
          [:td.td-modes
            [:.menu-modes
              (when author?
                (menu-mode "Edit" mode))
              (menu-mode "Read" mode)
              (menu-mode "Present" mode)]]
          [:td.td-theme
            (when author?
              [:.menu-theme [:div "Theme" [:span {:style {"float" "right"}} "▾"]]])]
          [:td.td-stats
            [:.menu-stats
              [:div
                [:.menu-stats-bullet
                  { :class (when (pos? spectators) "menu-stats-bullet_live") }]
                  (str spectators " watching live")]]]]]]))


(rum/defc deck-page < rum/reactive []
  (let [mode (rum/react *mode)]
    [:.page_deck
      (menu (rum/react *pending-deck) mode)
      (case mode
        "Edit"    (edit/edit-page *pending-deck)
        "Read"    (read/read-page *pending-deck)
        "Present" (present/present-page *pending-deck socket))]))


;; When patch comes from server
(defn on-server-push [patch]
  ;; Calculating what changes have we accumulated up to this point
  (let [delta (u/diff @*server-deck @*pending-deck)]
    ;; We apply it to confirmed deck first
    (swap! *server-deck u/patch patch)
    ;; Take new server-deck and apply local changes to it
    (reset! *pending-deck (u/patch @*server-deck delta))))


(defonce *deck-id (atom nil))


(defn refresh! [deck-id]
  (when-not @*deck-id
    (reset! *deck-id deck-id))

  (when socket
    (.close socket)
    (reset! *server-deck nil)
    (reset! *pending-deck nil))
  
  (println "Loading deck" deck-id)
  ;; TODO watch websocket status, reconnect
  (set! socket
    (doto (js/WebSocket. (str "ws://" js/location.host "/api/deck/" deck-id))
      (aset "onmessage"
        (fn [payload]
          (-> (.-data payload)
              (u/transit->obj )
              :patch
              (on-server-push))))))
           

  (rum/mount (deck-page) (js/document.getElementById "app")))


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
