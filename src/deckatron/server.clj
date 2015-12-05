(ns deckatron.server
  (:use [compojure.core          :only [defroutes GET POST]]
        [ring.middleware.cookies :only [wrap-cookies]])
  (:require
    [clojure.java.io    :as io]
    [compojure.route    :as route]
    [deckatron.storage  :as storage]
    [deckatron.util     :as u]
    [org.httpkit.server :as httpkit]
    [ring.util.response :as response])
  (:gen-class))

; # Types (please, reflect what's implementated)
;
; Deck {:deck/id       <uuid-string>
;       :deck/content  <string>
;       :user/id       <uuid-string>}


; command {:op <keyword> -- see bellow
;          + :command-dependent-args ...}
;
; Commands:
;         {:op :deck/get  :deck/id <uuid>}                        => Deck
;
; Commands that require "user-id" (via cookie):
;         {:op :deck/new}                                         => Deck (with a newly generated :deck/id)
;         {:op :deck/save :deck/id <uuid> :deck/content <string>} => nil
;         {:op :deck/list-mine}                                   => [Deck]
;
(defn apply-api-command [user-id c]
  (condp = (:op c)
    :deck/get       (storage/deck-get (:deck/id c))
    :deck/new       (storage/deck-new  user-id)
    :deck/save      (storage/deck-save user-id (:deck/id c) (:deck/content c))
    :deck/list-mine (storage/deck-list-by-user user-id)
    (u/die (str "Unknown :op \"" (:op c) \") {:command c})))


(defroutes routes
  (GET "/" [] (response/resource-response "public/index.html"))
  (GET "/api/websocket" [:as req]
    (let [user-id (get-in req [:cookies "user-id" :value])]
      (httpkit/with-channel req chan
        (u/spy "+++" user-id)

        (httpkit/on-close chan
          (fn [status]
            (u/spy "---" user-id)))

        (httpkit/on-receive chan
          (fn [bytes]
            (->>
              (try (->> bytes
                     u/transit->obj
                     (u/spy "<")
                     (apply-api-command user-id))
                   (catch Exception e
                     (str "Exception: " (.getMessage e)))) ; TODO serialize + raise a real exception on client

              (u/spy ">")
              u/obj->transit
              (httpkit/send! chan))))))) ; NOTE: no need to care if an ex. thrown here -- can't do anything anyways

  (route/resources "/" {:root "public"}))

(defn set-user-id-cookie-if-absent [handler]
  (fn [req] ; TODO set a single path for all paths?
    (let [user-id (get-in req [:cookies "user-id" :value])]
      (assoc-in (handler req) [:cookies "user-id"] {:value   (or user-id (u/uuid))
                                                    :max-age (* 10 365 24 60 60)}))))

(def app (-> routes set-user-id-cookie-if-absent wrap-cookies))

(defn -main [& args]
  (println "Starting server at port 8080")
  (httpkit/run-server #'app {:port 8080}))
