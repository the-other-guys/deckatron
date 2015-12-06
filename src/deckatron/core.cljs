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


(defonce parsed-deck-cache (atom {})) ;; {unparsed-slide parsed-slide}


(defn parse-with-cache! [cache vs]
  (let [keys-to-drop  (clojure.set/difference (set (keys @cache)) (set vs))
        _             (apply (partial swap! cache dissoc) keys-to-drop)
        keys-to-parse (clojure.set/difference (set vs) (keys @cache))
        parsed        (mapv #(vec [% (p/parse %)]) keys-to-parse)]
    (print (str "dropped from cache: " (count keys-to-drop) "\n" keys-to-drop))
    (print (str "added to cache: "(count keys-to-parse) "\n" keys-to-parse))
    (print (str "total in cache: "(count (keys @cache))))
    (doseq [[k v] parsed]
      (swap! cache assoc k v))))

(defn- ->slides [txt]
  (print (str "cache before: " @parsed-deck-cache))
  (let [unparsed (->> txt p/split-text-into-slides)
        _        (parse-with-cache! parsed-deck-cache unparsed)
        slides   (mapv #(get @parsed-deck-cache %) unparsed)]
    (print (str "cache after: " @parsed-deck-cache))
    slides))

(defn ->slides-only [txt]
  (->> txt ->slides (filter #(= :slide (:s/type %)))))

(defn ->slides-and-notes [txt]
  (->slides txt))

(defn slide [s]
   (layouts/render-slide s))

(defn ->first-slide-only [txt]
  (->> txt
    p/split-text-into-slides
    (filter #(= :slide (:s/type %)))
    first
    p/parse))

(defn presenting? [deck]
  (and (:presenter-slide deck)
       (contains? (:deck/spectators deck) (:user/id deck))))
