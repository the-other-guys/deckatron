(ns deckatron.core
  (:require
    [deckatron.app :as app]))

(def user-id (second (re-find #"user-id=([a-z0-9\-]+)" js/document.cookie)))

(def user-deckatron "user-deckatron")

(enable-console-print!)
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
  (.preventDefault e)
  (js/history.pushState nil nil href)
  (app/refresh))