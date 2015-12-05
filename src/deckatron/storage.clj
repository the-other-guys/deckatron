(ns deckatron.storage
  (:require
    [clojure.java.io    :as io]
    [clojure.string     :as str]
    [deckatron.util     :as u]))

; ~/.deckatron
;  + users
;    + <user-id>          <-- data owned by user
;      + decks            <-- owned decks
;        - <deck-id>.md
;        - <deck-id>.md
;        - <deck-id>.md
;
;  + decks                <-- all decks index (symlinks)
;    - <id>.md
;    - <id>.md
;    - <id>.md

; dir  = directory
; file = file or directory
; link = symlink
;
(defn  dir-home       []                (io/file (System/getProperty "user.home") ".decaktron"))
(defn  dir-users      []                (io/file (dir-home) "users"))
(defn  dir-user       [user-id]         (io/file (dir-users) user-id))
(defn  dir-user-decks [user-id]         (io/file (dir-user user-id) "decks"))
(defn file-user-deck  [user-id deck-id] (io/file (dir-user-decks user-id) (str deck-id ".md")))

; indexes
(defn  dir-decks      []                (io/file (dir-home) "decks"))
(defn link-deck       [deck-id]         (io/file (dir-home) "decks" (str deck-id ".md")))

(defn symlink [link target]
  (java.nio.file.Files/createSymbolicLink
    (.toPath link)
    (.toPath target)
    (into-array java.nio.file.attribute.FileAttribute [])))

(defn spit-mkdirs [f content]
  (io/make-parents f)
  (spit f content))

(defn mk-deck [user-id]
  {:deck/id (u/ssid)
   :user/id user-id
   :deck/content (str/join "\n"
                          ["# About me"
                           ""
                           "- a writer"
                           "- a doctor"
                           "- a philisopher"
                           "- a man (or a mouse https://youtu.be/IdnMJ7aB9OU)"])})

(defn deck-new [user-id]
  (let [deck      (mk-deck user-id)
        deck-file (file-user-deck user-id (:deck/id deck))
        deck-link (link-deck (:deck/id deck))]
    (spit-mkdirs deck-file (u/obj->transit deck))
    (io/make-parents deck-link)
    (symlink deck-link deck-file)
    deck))

(defn deck-get [deck-id]
  (-> (link-deck deck-id) slurp u/transit->obj))

(defn deck-save [user-id deck-id deck-content]
  (let [deck-file (file-user-deck user-id deck-id)
        _         (if-not (.exists deck-file)
                    (u/die "Can't save not owned deck"))
        old-deck  (deck-get deck-id)
        new-deck  (assoc old-deck :deck/conent deck-content)
        _         (spit deck-file (u/obj->transit new-deck))]))

(defn deck-list-by-user [user-id]
  (map (comp u/transit->obj slurp)
    (seq (.listFiles (dir-user-decks user-id)))))
