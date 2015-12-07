(ns deckatron.pages.layouts
  (:require
    [rum.core :as rum]
    [deckatron.util :as u]
    [clojure.string :as str]))

(defn tg->html [tg]
  (let [types (:e/types tg)
        text  (:e/text tg)]
    (cond
      (contains? types :link)   (if-let [youtube-id (second (re-find #"www.youtube.com/watch\?v=(.+)" (:e/href tg)))]
                                  ; TODO extract URL parts: host name and "v" argument
                                  [:iframe.slide-fill
                                    { :src            (str "https://www.youtube.com/embed/" youtube-id)
                                      :frameborder     0
                                      :allowfullscreen true }]
                                  [:a {:href (:e/href tg) :target "_blank"} text])

      (contains? types :strong) [:strong {} text]
      (contains? types :em)     [:em {} text]

      (contains? types :image)  [:div.slide-fill 
                                  { :style { :background-size "contain"
                                             :background-repeat "no-repeat"
                                             :background-position "center center"
                                             :background-image (str "url('" (:e/href tg) "')")
                                             :title text } }]

      :else                     [:span {} text])))

(defn- parse-span [sp]
  (map tg->html (:l/elements sp)))

(defn- parse-tx [lines]
  (map parse-span lines))

(defn- parse-li [lines]
  (map (fn [%] [:li (parse-span %)]) lines))

(defn- parse-blockquote [lines]
  (map (fn [%] [:blockquote (parse-span %)]) lines))

(defn- parse-pre [lines]
  (str/join "\n" (map (fn [l] (str/join (map :e/text (:l/elements l)))) lines)))

(defn- parse-p [p]
  (case (:p/type p)
    :h1 [:h1 (parse-tx (:p/lines p))]
    :h2 [:h2 (parse-tx (:p/lines p))]
    :h3 [:h3 (parse-tx (:p/lines p))]
    :h4 [:h4 (parse-tx (:p/lines p))]
    :ordered-list [:ol (parse-li (:p/lines p))]
    :unordered-list [:ul (parse-li (:p/lines p))]
    :blockquote (parse-blockquote (:p/lines p))
    :code [:pre (parse-pre (:p/lines p))]
    [:div (parse-tx (:p/lines p))]
    )
  )

(defn- ->html [slide]
  (map parse-p slide))

(rum/defc notes-layout [ast]
  [:.note
    [:.note-inner
      (->html ast)]])


(rum/defc default-layout [slide]
  [:.slide
   [:.slide-inner
    [:.slide-text (->html slide)]]])

; e (-> slide first :p/lines first :l/elements first :e/text)
(rum/defc head-only-layout [slide]
  (let [t (-> slide first :p/type)]
    [:.slide
     [:.slide-inner
      [:.slide-text
       (->html slide)]]]))


(rum/defc two-headed-centered-layout [slide]
  (let [t1 (-> slide first :p/type)
        t2 (-> slide second :p/type)
        e1 (-> slide first :p/lines first :l/elements first)
        e2 (-> slide second :p/lines first :l/elements first)]
    [:.slide
     [:.slide-inner
      [:.slide-text
       (->html slide)]]]))


(def LAYOUTS
  {[:h1] head-only-layout
   [:h2] head-only-layout
   [:h3] head-only-layout
   [:h4] head-only-layout
   [:h1 :h2] two-headed-centered-layout
   [:h1 :h3] two-headed-centered-layout
   [:h1 :h4] two-headed-centered-layout
   [:h2 :h1] two-headed-centered-layout
   [:h2 :h3] two-headed-centered-layout
   [:h2 :h4] two-headed-centered-layout})


(defn- ->layout [slide]
  ;;   (print (str "slide: " slide))
  (if (= :notes (:s/type slide))
    notes-layout
    (let [key    (mapv :p/type (:s/paragraphs slide))
          layout (get LAYOUTS key default-layout)]
      ;;       (print (str "layout key: " key))
      layout)))


(defn render-slide [slide]
  (let [layout (->layout slide)]
    (layout (:s/paragraphs slide))))
