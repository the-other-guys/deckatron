(ns deckatron.core
  (:require
    [rum.core :as rum]
    [clojure.string :as str]))


(enable-console-print!)


(def user-id (second (re-find #"user-id=([a-z0-9\-]+)" js/document.cookie)))
(println ":user/id" user-id)


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


(defn slides [content]
  (str/split content #"(?:---|===)"))


(rum/defc slide [slide]
  [:.slide
    [:.slide-inner
      [:.slide-text (str/trim slide)]]])
