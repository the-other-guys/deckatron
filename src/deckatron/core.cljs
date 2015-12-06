(ns deckatron.core
  (:require
    ;[deckatron.app :as app]
    [rum.core :as rum]
    [clojure.string :as str]
    [deckatron.parser :as p]
    [deckatron.pages.layouts :as layouts]))


(def user-deckatron "user-deckatron")

(enable-console-print!)


(def user-id (second (re-find #"user-id=([a-z0-9\-]+)" js/document.cookie)))
(println ":user/id" user-id)

(defn ->deck-id []
  (second (re-matches #"/deck/(.+)/(.+)" js/window.location.pathname)))

(defn ->mode []
  (nth (re-matches #"/deck/(.+)/(.+)" js/window.location.pathname) 2))

(defn author? [deck]
  (= user-id (:user/id deck)))

(defn ->default-mode [deck]
  (condp = (:user/id deck)
    user-id "Edit"
    user-deckatron "Read"
    "Read"))

(defn ->deck-href
  ([deck]      (->deck-href deck (->default-mode deck)))
  ([deck mode] (str "/deck/" (:deck/id deck) "/" mode)))

(defn fake-navigate-url [href e]
  (when e
    (.preventDefault e))
  (js/history.pushState nil nil href)
  (js/window.onpopstate))

(def aspect (/ 16 9))

(defonce *window-width (atom 0))
(defonce *window-height (atom 0))


(set! js/window.onresize
  (fn [_]
    (let [w js/document.documentElement.clientWidth
          h js/document.documentElement.clientHeight]
      (when (not= @*window-width w)
        (reset! *window-width w))
      (when (not= @*window-height h)
        (reset! *window-height h)))))


(js/window.onresize)


(defmulti start-page! (fn [path mount-el] (first path)))
(defmulti stop-page! (fn [path] (first path)))


(defn slides [txt]
  (str/split txt #"(?:---|===)")
  #_(let [pages (->> txt
                   p/split-text-into-slides
                   (mapv p/parse))]
    (println (str "pages:" (prn-str pages)))
    pages))


;; (defn slide [s]
;;   (layouts/slide->layout s))

(rum/defc slide [slide]
  [:.slide
    [:.slide-inner
      [:.slide-text (str/trim slide)]]])
