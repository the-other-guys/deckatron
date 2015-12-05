(ns deckatron.util
  (:require
    [deckatron.util     :as u]
    [cognitect.transit  :as t]))

(defn ssid
  "Generate semi-sequential id based on current time and 22 bit random number,
   encoded as 11-char 'safe' ASCII string. E.g. (ssid) => 'FRcSwaV0Bkv'"
  []
  (let [id    (-> (.getTime (java.util.Date.))
                  (bit-shift-left 22)
                  (bit-or (rand-int 0x800000)))
        table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"]
    (loop [id id
           res ""]
      (if (== 0 id)
        res
        (let [digit (bit-and id 0x3F)]
          (recur (bit-shift-right id 6) (str (nth table digit) res)))))))


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
