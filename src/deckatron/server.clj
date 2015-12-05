(ns deckatron.server
  (:use [compojure.core          :only [defroutes GET POST]]
        [ring.middleware.cookies :only [wrap-cookies]])
  (:require
    [compojure.route    :as route]
    [org.httpkit.server :as httpkit]
    [ring.util.response :as response]
    [cognitect.transit  :as t])
  (:gen-class))


(defn read-transit-str [s]
  (-> s
      (.getBytes "UTF-8")
      (java.io.ByteArrayInputStream.)
      (t/reader :json)
      (t/read)))


(defn write-transit-str [o]
  (let [os (java.io.ByteArrayOutputStream.)]
    (t/write (t/writer os :json) o)
    (String. (.toByteArray os) "UTF-8")))


(defn uuid [] (str (java.util.UUID/randomUUID)))

(defroutes routes
  (GET "/" [] (response/resource-response "public/index.html"))
  (GET "/api/websocket" [:as req]
    (httpkit/with-channel req chan

      (println "Connected from user-id" (get-in req [:cookies "user-id" :value]))

      (httpkit/on-close chan
        (fn [status]
          (println "Disconnected")))

      (httpkit/on-receive chan
        (fn [payload]
          (let [message (read-transit-str payload)]
            (println "Received:" message)
            (httpkit/send! chan (write-transit-str ["pong" message])))))))

  (route/resources "/" {:root "public"}))

(defn set-user-id-cookie-if-absent [handler]
  (fn [req] ; TODO set a single path for all paths?
    (let [user-id (get-in req [:cookies "user-id" :value])]
      (assoc-in (handler req) [:cookies "user-id"] {:value   (or user-id (uuid))
                                                    :max-age (* 10 365 24 60 60)}))))

(def app (-> routes set-user-id-cookie-if-absent wrap-cookies))

(defn -main [& args]
  (println "Starting server at port 8080")
  (httpkit/run-server #'app {:port 8080}))
