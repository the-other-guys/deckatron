(ns deckatron.app
  (:require
    [deckatron.core :as core]
    [deckatron.pages.home :as home]
    [deckatron.pages.deck :as deck]))


(enable-console-print!)


(defn parse-path [path]
  (condp re-matches path
    #"/"                         [:home]
    #"/deck/([^/]+)"         :>> (fn [[_ id mode]] [:deck id])
    #"/deck/([^/]+)/([^/]+)" :>> (fn [[_ id mode]] [:deck id mode])))


(def *last-path (atom nil))


(defn render []
  (let [old @*last-path
        new (parse-path js/window.location.pathname)]
    (when old
      (core/stop-page! old new))
    (core/start-page! new (js/document.getElementById "app"))
    (reset! *last-path new)))


(defn ^:export refresh []
  (set! js/document.body.className "")
  (set! js/window.onerror #(set! js/document.body.className "err"))
  (set! js/window.onpopstate render)
  (render))
