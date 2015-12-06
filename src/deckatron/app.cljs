(ns deckatron.app
  (:require
    [deckatron.pages.home :as home]
    [deckatron.pages.deck :as deck]))


(enable-console-print!)


(defn ^:export refresh []
  (condp re-matches js/window.location.pathname
    #"/"              (home/refresh!)
    #"/deck/(.+)/(.+)" :>> (fn [[_ deck-id mode]] (deck/refresh! deck-id))))
