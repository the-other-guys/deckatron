(ns deckatron.util
  (:require
    clojure.data
    [cognitect.transit :as t]))

(defn ssid
  "Generate semi-sequential id based on current time and 32 bit random number,
   encoded as 14-char 'safe' ASCII string. E.g. (ssid) => 'blgfojbacbuvnt'"
  ([] (ssid nil))
  ([prefix]
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
      (str prefix (stringify high) (stringify low)))))


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


(defn diff [a b]
  (let [[a b _] (clojure.data/diff a b)]
    [a b]))
   
   
(defn- patch-delete [o delta]
  (cond
    (set? delta) (clojure.set/difference o delta)
    (map? delta) (reduce-kv (fn [o k v] 
                              (if (= (get o k) v)
                                (dissoc o k)
                                (update o k patch-delete v))) o delta)
    :else (throw (ex-info "Unsupported container: " delta))))


(defn- patch-add [o delta]
  (cond
    (set? delta) (clojure.set/union o delta)
    (map? delta) (reduce-kv (fn [o k v]
                              (if (contains? o k)
                                (update o k patch-add v)
                                (assoc o k v))) o delta)
   :else (throw (ex-info "Unsupported container: " delta))))


(defn patch [o diff]
  (let [[minus plus] diff]
    (if (nil? o)
      plus
      (cond-> o
        minus (patch-delete minus)
        plus  (patch-add plus)))))


(defn width->font-size [w]
  (-> w (/ 440) (* 10)))
