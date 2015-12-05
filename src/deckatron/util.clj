(ns deckatron.util
  (:require
    [deckatron.util     :as u]
    [cognitect.transit  :as t]))

(defn uuid []            (str (java.util.UUID/randomUUID)))
(defn die  [message map] (throw (ex-info message map)))
(defn spy  [prefix o]    (println prefix o) o)

(defn transit->obj [s]
  (-> s
    (.getBytes "UTF-8")
    (java.io.ByteArrayInputStream.)
    (t/reader :json)
    (t/read)))

(defn obj->transit [o]
  (let [os (java.io.ByteArrayOutputStream.)]
    (t/write (t/writer os :json) o)
    (String. (.toByteArray os) "UTF-8")))
