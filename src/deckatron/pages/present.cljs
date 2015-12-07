(ns deckatron.pages.present
  (:require
    [clojure.string :as str]
    [rum.core :as rum]
    [deckatron.core :as core]
    [deckatron.util :as u]
    [deckatron.parser :as parser]))


(enable-console-print!)


(def *kb-active? (atom true))


(def listen-keyboard
  { :did-mount
    (fn [state]
      (let [[slides *slide _ spectator?] (:rum/args state)
            max-idx (count slides)]
        (when-not spectator?
          (reset! *kb-active? true)
          (set! js/document.onkeydown
            (fn [e]
              (when @*kb-active?
                (let [current @*slide
                      idx     (condp contains? (.-keyCode e)
                                #{8 37 38 33 80}  (dec current) ;; backspace, left, up, pageup, p
                                #{13 32 39 40 34} (inc current) ;; enter, space, right, down, pagedown
                                #{36}             0             ;; home
                                #{35}             (dec max-idx) ;; end
                                nil)]
                  (when idx
                    (let [idx (-> idx (max 0) (min max-idx))]
                      (when (not= current idx)
                        (reset! *slide idx)))
                    (.preventDefault e))))))))
      state)
    :will-unmount
    (fn [state]
      (set! js/document.onkeydown nil)
      state) })


(defn- send-question! [text *deck]
  (let [qid (u/ssid "q-")
        q   { :question/id  qid
              :deck/id      (:deck/id @*deck)
              :user/id      core/user-id
              :question/text text
              :question/created-at (js/Date.) }]
    (swap! *deck update :deck/questions assoc qid q)))


(rum/defc qs [*deck class]
  [:div { :class class }
   (for [q (->> (vals (:deck/questions @*deck))
                (sort-by (juxt #(- (count (:question/upvoters %))) :question/created-at :question/id)))
         :let [path [:deck/questions (:question/id q) :question/upvoters]
               voted? (contains? (:question/upvoters q) core/user-id)]]
     [:.q
      (:question/text q)
      [:.q-likes 
       { :class (when voted? "q-likes_voted")
         :on-click (fn [e]
                     (swap! *deck update-in path (fnil conj #{}) core/user-id)) }
       [:span.q-heart]
       (count (:question/upvoters q))]
      [:.q-t]])])


(rum/defc slides < rum/reactive, listen-keyboard [slides *slide *deck]
  [:.slides
    (let [idx (rum/react *slide)]
      (if (== idx (count slides))
        [:.q-slide
          (if (pos? (count (:deck/questions @*deck)))
            (list
              [:h1 "Your questions:"]
              (qs *deck ""))
            [:h1 "There’re no questions. Thank you!"])]
        (core/slide (nth slides idx nil))))])


(def set-starting-slide
  { :will-mount
    (fn [state] 
      (let [[*deck spectator?] (:rum/args state)]
        (when-not spectator?
          (swap! *deck assoc :presenter-slide 0)))
      state)
    :will-unmount
    (fn [state] 
      (let [[*deck spectator?] (:rum/args state)]
        (when-not spectator?
          (swap! *deck dissoc :presenter-slide)))
      state) })
  

(rum/defcs questions < (rum/local false ::open) [{*open? ::open} *deck]
  [:.questions
    [:.q-icon { :on-click (fn [e] (swap! *open? not)) }
      [:.q-icon-inner "?"]]
    (when @*open?
      [:.q-popup
        [:.q-popup-inner
          (qs *deck "q-questions")
          [:textarea.q-ta
           { :placeholder "Your question..."
             :on-focus (fn [_] (reset! *kb-active? false))
             :on-blur  (fn [_] (reset! *kb-active? true))
             :on-key-down (fn [e]
                            (when (and (= 13 (.-keyCode e))
                                       (not (.-shiftKey e)))
                              (.preventDefault e)
                              (send-question! (.. e -target -value) *deck)
                              (aset (.-target e) "value" "")))
           }]]])])


(defn fade
  ([] (fade true))
  ([bool] (set! js/document.body.className (if bool "faded" ""))))


(def fade-mixin
  { :will-mount
    (fn [s]
      (let [*t (atom nil)
            mm (fn [_]
                 (fade false)
                 (js/clearTimeout @*t)
                 (reset! *t (js/setTimeout fade 2000)))]
        (.addEventListener js/document "mousemove" mm false)
        (mm nil)
        (assoc s ::fade-timer *t
                 ::fade-handler mm)))
    
    :transfer-state
    (fn [o n]
      (merge n (select-keys o [::fade-timer ::fade-handler])))
    
    :will-unmount
    (fn [s]
      (js/clearTimeout @(::fade-timer s))
      (fade false)
      (.removeEventListener js/document "mousemove" (::fade-handler s))
      s)})
  

(rum/defc present-page < rum/reactive
                         set-starting-slide
                         fade-mixin
  [*deck spectator?]
  (let [deck   (rum/react *deck)
        width  (rum/react core/*window-width)
        height (rum/react core/*window-height)
        max-w  (* height core/aspect)
        w      (min width max-w)
        h      (/ w core/aspect)]
    [:.page_deck-present
      { :class (:deck/theme deck "default")
        :style { :padding-top  (-> (- height h) (/ 2) (str "px"))
                 :padding-left (-> (- width w) (/ 2) (str "px"))
                 :font-size    (str (u/width->font-size w false) "px")
                 :height       height} }
      (questions *deck)
      (when-let [content (:deck/content deck)]
        (slides (core/->slides-only content) (rum/cursor *deck [:presenter-slide]) *deck spectator?))]))
