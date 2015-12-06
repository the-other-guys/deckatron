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
; Deck {:deck/id         <id-string>
;       :deck/content    <string>
;       :user/id         <id-string>
;       :deck/viewed-by  #{<user-ids}
;       :deck/spectators #{<user-ids}
; }


(defn send-obj! [chan o]
  (println ">>> sent:     " o)
  (httpkit/send! chan (u/obj->transit o)))

(defn new-patch-message [deck-id patch]
  {:deck/id deck-id
   :patch   patch})

;; broadcasting
(def *subject->subscribers (atom {})) ; Object -> #{ {:user-id <id-string> :chan Channel} }

(defn listen [subject user-id chan]
  (println "+++ listen:   " subject user-id)
  (swap! *subject->subscribers update subject
    #(conj (or % #{}) {:user-id user-id :chan chan})))

(defn unlisten [subject user-id chan]
  (println "--- unlisten: " subject user-id)
  (swap! *subject->subscribers
    (fn [m]
      (let [new-chans (disj (get m subject) {:user-id user-id :chan chan})]
        (if (empty? new-chans)
          (dissoc m subject)
          (assoc  m subject new-chans))))))

(defn broadcast!
  ([subject message]             (broadcast! subject message nil))
  ([subject message ignore-chan] (doseq [s (get (deref *subject->subscribers) subject)
                                        :when (not= ignore-chan (:chan s))]
                                   (println "!!! broadcast:" subject message)
                                   (send-obj! (:chan s) message))))

(defn update-counters-and-broadcast [deck-id user-id]
  (->>
    (storage/update-and-eject-diff! deck-id
      (fn [deck]
        (-> deck
          (update :deck/viewed-by  #(conj (or % #{}) user-id))
          (assoc  :deck/spectators (->> (get (deref *subject->subscribers) deck-id)
                                     (map :user-id)
                                     distinct
                                     set)))))
    (new-patch-message deck-id)
    (broadcast! deck-id)))

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

        (listen :decks user-id chan)

        (httpkit/on-close chan
          (fn [status]
            (println "Disconnected" user-id "from ALL")
            (unlisten :decks user-id chan)))
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

        ; TODO a possbile racing condition: there could be missing diffs
        ;      between the moments when the initial snapshot was sent and
        ;      when the subscription was registered
        (listen deck-id user-id chan)

        (update-counters-and-broadcast deck-id user-id)

        (httpkit/on-close chan
          (fn [status]
            (println "Disconnected" user-id "from" deck-id)
            (unlisten deck-id user-id chan)

            (update-counters-and-broadcast deck-id user-id)))

        (httpkit/on-receive chan
          (fn [bytes]
            ;; applying patch
            (let [{:keys [patch]} (u/transit->obj bytes)
                  old             (storage/get-deck deck-id)
                  patch-message   (new-patch-message deck-id patch)]

              (when (not= (:user/id old) user-id)
                (u/die "Access denied" { :deck/id deck-id, :user/id user-id }))

              (println "Updating" deck-id)

              ; TODO consider abstracting away storage + broadcasting behind a single thing
              (storage/patch-deck! deck-id patch)
              (broadcast! deck-id patch-message chan) ; don't send back to self (chan)
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
