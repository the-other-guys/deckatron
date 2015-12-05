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
; Deck {:deck/id       <id-string>
;       :deck/content  <string>
;       :user/id       <id-string>}


(defn- send-patch! [chan deck-id patch]
  (->> { :deck/id deck-id
         :patch   patch }
       (u/obj->transit)
       (httpkit/send! chan)))


(defroutes routes
  
  ;; Home page
  
  (GET "/" []
    (response/resource-response "public/index.html"))
  
  
  ;; Deck page
  
  (GET "/deck/:id" []
    (response/resource-response "public/index.html"))
  
  
  ;; redirect to /deck/:id page
  
  (GET "/create-deck" [:as req]
    (let [user-id (:user/id req)
          deck    (storage/create-deck! user-id)]
       { :status  302
         :headers {"Location" (str "/deck/" (:deck/id deck))}}))
  
  
  ;; on connect -> { :deck/id ..., :patch ... } (separate msg for each deck)
  ;; when deck changed by someone else -> { :deck/id ..., :patch ... }
  
  (GET "/api/decks" [:as req]
    (let [user-id (:user/id req)]
      (httpkit/with-channel req chan
        (println "Connected" user-id "to ALL")
        ;; initial payload
        (doseq [deck (storage/all-decks)]
          (send-patch! chan (:deck/id deck) (u/diff nil deck)))
        ;; TODO register user in broadcast map
        (httpkit/on-close chan
          (fn [status]
            (println "Disconnected" user-id "from ALL")
            ;; TODO remove from broadcast list
            ))
        ;; nothing to receive
        )))
  
  
  ;; on connect                        -> { :deck/id ..., :patch ... }
  ;; when deck changed by someone else -> { :deck/id ..., :patch ... }
  ;; when deck changed by this user    <- { :deck/id ..., :patch ... }
  
  (GET "/api/deck/:deck-id" [deck-id :as req]
    (let [user-id (:user/id req)]
      (httpkit/with-channel req chan
        (println "Connected" user-id "to" deck-id)
        
        ;; initial payload
        (let [deck (storage/get-deck deck-id)]
          (send-patch! chan deck-id (u/diff nil deck)))
        
        ;; TODO register user in broadcast map
        ;; TODO add spectator + broadcast

        (httpkit/on-close chan
          (fn [status]
            (println "Disconnected" user-id "from" deck-id)
            ;; TODO remove from broadcast list
            ;; TODO remove spectator + broadcast
            ))

        (httpkit/on-receive chan
          (fn [bytes]
            ;; applying patch
            (let [{:keys [patch]} (u/transit->obj bytes)
                  old (storage/get-deck deck-id)]
              (when (not= (:user/id old) user-id)
                (u/die "Access denied" { :deck/id deck-id, :user/id user-id }))
              (println "Updating deck" deck-id)
              (storage/update-deck! deck-id patch)
              ;; TODO broadcast
              ))))))

  (route/resources "/" {:root "public"}))


(defn ensure-userid [handler]
  (fn [req] ; TODO set a single path for all paths?
    (let [user-id (or (get-in req [:cookies "user/id" :value])
                      (u/ssid "user-"))]
          (-> req
              (assoc :user/id user-id)
              (handler)
              (assoc-in [:cookies "user/id"] {:value   user-id
                                              :max-age (* 10 365 24 60 60)})))))


(def app (-> routes ensure-userid wrap-cookies))

(defn -main [& args]
  (println "Starting server at port 8080")
  (httpkit/run-server #'app {:port 8080}))
