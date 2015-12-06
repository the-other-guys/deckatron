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

; # Types (please, keep up to date with current implementation)
;
; Deck {:deck/id       <id-string>
;       :deck/content  <string>
;       :user/id       <id-string>}


(defn send-obj! [chan o]
  (httpkit/send! chan (u/obj->transit o)))

(defn new-patch-message [deck-id patch]
  {:deck/id deck-id
   :patch   patch})

;; broadcasting
(def *subject->chans (atom {})) ; Object -> #{Channel}

(defn listen [subject chan]
  (println "+++ " subject chan)
  (swap! *subject->chans update subject #(conj (or % #{}) chan)))

(defn unlisten [subject chan]
  (println "--- " subject chan)
  (swap! *subject->chans
    (fn [m]
      (let [new-chans (disj (get m subject) chan)]
        (if (empty? new-chans)
          (dissoc m subject)
          (assoc  m subject new-chans))))))

(defn broadcast!
  ([subject message]             (broadcast! subject message nil))
  ([subject message ignore-chan] (doseq [chan (get (deref *subject->chans) subject)
                                        :when (not= chan ignore-chan)]
                                   (println "!!! " chan message)
                                   (send-obj! chan message))))



(defroutes routes

  ;; Home page

  (GET "/" []
    (response/resource-response "public/index.html"))


  ;; Deck page

  (GET "/deck/:id" []
    (response/resource-response "public/index.html"))

  (GET "/deck/:id/:mode" []
    (response/resource-response "public/index.html"))
  
  ;; redirect to /deck/:id page

  (GET "/create-deck" [:as req]
    (let [user-id (:user/id req)
          deck    (storage/create-deck! user-id)
          deck-id (:deck/id deck)]
       (println "Created" deck-id)
       { :status  302
         :headers {"Location" (str "/deck/" deck-id "/Edit")}}))
  
  
  ;; on connect -> { :deck/id ..., :patch ... } (separate msg for each deck)
  ;; when deck changed by someone else -> { :deck/id ..., :patch ... }

  (GET "/api/decks" [:as req]
    (let [user-id (:user/id req)]
      (httpkit/with-channel req chan
        (println "Connected" user-id "to ALL")

        ;; initial payload
        (doseq [deck (storage/all-decks)]
          (send-obj! chan (new-patch-message (:deck/id deck) (u/diff nil deck))))

        (listen :decks chan)

        (httpkit/on-close chan
          (fn [status]
            (println "Disconnected" user-id "from ALL")
            (unlisten :decks chan)))
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
          (send-obj! chan (new-patch-message deck-id (u/diff nil deck))))

        (listen deck-id chan)

        (httpkit/on-close chan
          (fn [status]
            (println "Disconnected" user-id "from" deck-id)
            (unlisten deck-id chan)))

        (httpkit/on-receive chan
          (fn [bytes]
            ;; applying patch
            (let [{:keys [patch]} (u/transit->obj bytes)
                  old             (storage/get-deck deck-id)
                  patch-message   (new-patch-message deck-id patch)]

              (when (not= (:user/id old) user-id)
                (u/die "Access denied" { :deck/id deck-id, :user/id user-id }))

              (println "Updating" deck-id)
              (storage/update-deck! deck-id patch)

              (broadcast! deck-id patch-message chan)
              (broadcast! :decks  patch-message patch)))))))

  (route/resources "/" {:root "public"}))


(defn ensure-userid [handler]
  (fn [req]
    (let [user-id (or (get-in req [:cookies "user-id" :value])
                      (u/ssid "user-"))]
          (-> req
              (assoc :user/id user-id)
              (handler)
              (assoc-in [:cookies "user-id"] {:value   user-id
                                              :max-age (* 10 365 24 60 60)})))))


(def app (-> routes ensure-userid wrap-cookies))

(defn -main [& args]
  (println "Starting server at port 8080")
  (httpkit/run-server #'app {:port 8080}))
