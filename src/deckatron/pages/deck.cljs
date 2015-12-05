(ns deckatron.pages.deck
  (:require-macros
    [cljs.core.async.macros :refer [go]])
  (:require
    [clojure.string :as str]
    [rum.core :as rum]
    [deckatron.core :as core]
    [deckatron.util :as u]
    [cljs.core.async :refer [put! chan <! >! timeout]]
    [deckatron.parser :as parser]))


(enable-console-print!)


(defonce socket nil)


(defonce *deck (atom nil))
(defonce *deck-id (atom nil))


(defonce *pending-content (atom nil))


(defonce *mode (atom nil))


(add-watch *deck ::default-mode
  (fn [_ _ old deck]
    (when (nil? old) ;; first load
      (if (= core/user-id (:user/id deck))
        (reset! *mode "Edit")
        (reset! *mode "Read")))
    (remove-watch *deck ::default-mode)))


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


(rum/defc page < rum/reactive []
  (let [deck  (rum/react *deck)
        mode  (rum/react *mode)
        value (or (rum/react *pending-content)
                  (:deck/content deck))
        width  (-> (rum/react core/*window-width) (/ 2))
        height (rum/react core/*window-height) ]
    [:.page_deck_edit
      (menu deck mode)
      
      #_[:div.hidden-editor
        { :style { :width (str width "px") } }
        (str value " ")]
     
      [:textarea.editor
        { :style     { :width  (str width "px")
                       :height (str height "px") }
          :value     value
          :on-change (fn [e]
                       (reset! *pending-content (.. e -target -value))) }]
      
      [:.slides
        { :style { :width  (str width "px")
                   :height (str height "px")
                   :font-size (-> width (/ 440) (* 10) (str "px")) } }
        (for [text (str/split value #"(?:---|===)")]
          (core/slide text))]]))


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

  (rum/mount (page) (js/document.getElementById "app")))


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
