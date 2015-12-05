(ns deckatron.util
  (:require
    [cognitect.transit :as t]))

(defn ssid
  "Generate semi-sequential id based on current time and 32 bit random number,
   encoded as 14-char 'safe' ASCII string. E.g. (ssid) => 'blgfojbacbuvnt'"
  []
  (let [high      (-> #?(:clj (.getTime (java.util.Date.))
                         :cljs (.getTime (js/Date.)))
                      (/ 1000)
                      (long))
        low       (long (rand 0x100000000))
        table     "abcdefghijklmnopqrstuvwxyz0123456"
        stringify (fn [x]
                    (loop [x   x
                           res ""]
                      (if (== 7 (count res)) ;; 32 bit, 5 bits per char
                        res
                        (let [digit (bit-and x 0x1F)]
                          (recur (unsigned-bit-shift-right x 5)
                                 (str (nth table digit) res))))))]
    (str (stringify high) (stringify low))))


(defn die [message map]
  (throw (ex-info message map)))


(defn spy [prefix o]
  (println prefix o)
  o)


(defn transit->obj [s]
#?(:clj 
     (-> s
       (.getBytes "UTF-8")
       (java.io.ByteArrayInputStream.)
       (t/reader :json)
       (t/read))
   :cljs
     (t/read (t/reader :json) s)))


(defn obj->transit [o]
#?(:clj
     (let [os (java.io.ByteArrayOutputStream.)]
       (t/write (t/writer os :json) o)
       (String. (.toByteArray os) "UTF-8"))
   :cljs
     (t/write (t/writer :json ) o)))
