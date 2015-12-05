(ns deckatron.server
  (:require
    [compojure.core :as compojure]
    [compojure.route :as route]
    [org.httpkit.server :as httpkit]
    [ring.util.response :as response]
    [cognitect.transit :as t])
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


(compojure/defroutes app
  (compojure/GET "/api/websocket" [:as req]
    (httpkit/with-channel req chan
      (println "Connected")

      (httpkit/on-close chan
        (fn [status]
          (println "Disconnected")))

      (httpkit/on-receive chan
        (fn [payload]
          (let [message (read-transit-str payload)]
            (println "Recieved:" message)
            (httpkit/send! chan (write-transit-str ["pong" message])))))))
  (compojure/GET "/" [] (response/resource-response "public/index.html"))
  (route/resources "/" {:root "public"}))


(defn -main [& args]
  (println "Starting server at port 8080")
  (httpkit/run-server #'app {:port 8080}))
