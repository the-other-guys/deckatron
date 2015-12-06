(ns deckatron.storage
  (:require
    [clojure.java.io    :as io]
    [clojure.string     :as str]
    [deckatron.util     :as u]))

; ~/.deckatron/
;    decks/
;      <deck-id>.edn
;      <deck-id>.edn
;      <deck-id>.edn

(def root-dir (io/file "./.deckatron"))
(def decks-dir (io/file root-dir "decks"))

(defn deck-file [deck-id]
  (io/file decks-dir (str deck-id ".edn")))

(defn spit-mkdirs [f content]
  (io/make-parents f)
  (spit f content))


(defn new-deck [user-id]
  {:deck/id (u/ssid "deck-")
   :user/id user-id
   :deck/content (str/join "\n"
                   ["# About me"
                    ""
                    "- a writer"
                    "- a doctor"
                    "- a philisopher"
                    "- a man (or a mouse https://youtu.be/IdnMJ7aB9OU)"])})


(defn get-deck [deck-id]
  (-> (deck-file deck-id) slurp u/transit->obj))


(defn- save-deck! [deck]
  (spit-mkdirs
    (deck-file (:deck/id deck))
    (u/obj->transit deck))
  deck)


(defn create-deck! [user-id]
  (let [deck (new-deck user-id)]
    (save-deck! deck)))


(defn patch-deck! [deck-id patch]
  ;; TODO verify deck hash?
  (let [old (get-deck deck-id)
        new (u/patch old patch)]
    (save-deck! new)))


(defn update-and-eject-diff! [deck-id f]
  (let [old  (get-deck deck-id)
        new  (f old)
        diff (u/diff old new)]
    (save-deck! new)
    diff))


(defn all-decks []
  (for [file  (seq (.listFiles decks-dir))
        :when (re-matches #".*\.edn" (.getName file))]
    (u/transit->obj (slurp file))))
        

  
