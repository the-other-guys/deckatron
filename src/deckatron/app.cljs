(ns deckatron.app
  (:require
    [deckatron.core :as core]
    [deckatron.pages.home :as home]
    [deckatron.pages.deck :as deck]))


(enable-console-print!)


(defn parse-path [path]
  (condp re-matches path
    #"/"              [:home]
    #"/deck/(.+)/(.+)" :>> (fn [[_ id mode]] [:deck id mode])))


(def *last-path (atom nil))


(defn ^:export refresh []
  (let [old @*last-path
        new (parse-path js/window.location.pathname)]
    (when old
      (core/stop-page! old new))
    (core/start-page! new (js/document.getElementById "app"))
    (reset! *last-path new)))


(set! js/window.onpopstate refresh)
