(ns deckatron.core
  (:require
    ;[deckatron.app :as app]
    [rum.core :as rum]
    [clojure.string :as str]
    [deckatron.parser :as p]
    [deckatron.pages.layouts :as layouts]))


(enable-console-print!)


(def user-id (second (re-find #"user-id=([a-z0-9\-]+)" js/document.cookie)))
(println ":user/id" user-id)


(defn author? [deck]
  (= user-id (:user/id deck)))


(defn go!
  ([href]
    (js/history.pushState nil nil href)
    (js/window.onpopstate))
  ([href e]
    (.preventDefault e)
    (go! href)))


(defn switch!
  ([href]
    (js/history.replaceState nil nil href)
    (js/window.onpopstate))
  ([href e]
    (.preventDefault e)
    (switch! href)))


(defn turbolink [url & [switch?]]
  { :href url
    :on-click (fn [e]
                (when (and (not (.-metaKey e))
                           (not (.-ctrlKey e)))
                  ((if switch? switch! go!) url e))) })


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
(defmulti stop-page! (fn [path next-path] (first path)))


(defn- ->slides [txt]
  (let [pages (->> txt
                   p/split-text-into-slides
                   (mapv p/parse))]
    (println (str "pages:" pages))
    pages))

(defn ->slides-only [txt]
  (-> txt ->slides (filter #(= :slide (:s/type %)))))

(defn ->slides-and-notes [txt]
  (->slides txt))

(defn slide [s]
   (layouts/slide->layout s))


(defn presenting? [deck]
  (and (:presenter-slide deck)
       (contains? (:deck/spectators deck) (:user/id deck))))
