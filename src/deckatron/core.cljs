(ns deckatron.core)

(def user-id (second (re-find #"user-id=([a-z0-9\-]+)" js/document.cookie)))

(enable-console-print!)
(println ":user/id" user-id)
