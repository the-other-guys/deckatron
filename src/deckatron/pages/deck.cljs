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
    [deckatron.pages.read :as read]))


(enable-console-print!)


(defonce socket nil)


(defonce *deck (atom nil))
(defonce *deck-id (atom nil))


(defonce *mode (atom nil))


(add-watch *deck ::default-mode
  (fn [_ _ old deck]
    (when (nil? old) ;; first load
      (if (= core/user-id (:user/id deck))
        (reset! *mode "Edit")
        (reset! *mode "Read")))
    (remove-watch *deck ::default-mode)))


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
      (menu (rum/react *deck) mode)
      (case mode
        "Edit" (edit/edit-page *deck socket)
        "Read" (read/read-page *deck)
        (edit/edit-page *deck socket))]))


(defn refresh! [deck-id]
  (when-not @*deck-id
    (reset! *deck-id deck-id))

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
;;             (println "Received:" data)
            (swap! *deck u/patch (:patch data)))))))

  (rum/mount (deck-page) (js/document.getElementById "app")))


(def TEST-CONNECTION-INTERVAL 5000)

(defn socket-closed? [s]
  (if-not s
    true
    (= 3 (.-readyState socket))))

(go
  (while true
    (<! (timeout TEST-CONNECTION-INTERVAL))
    (if (socket-closed? socket)
      (do
        (println (str "connection died, reconnecting with deck-id: " @*deck-id))
        (refresh! @*deck-id))
      (println (str "connection is alive, deck-id: " @*deck-id ", socket status: " (.-readyState socket))))))
