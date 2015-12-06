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
      (let [[slides *slide] (:rum/args state)
            max-idx         (dec (count slides))]
        (reset! *kb-active? true)
        (set! js/document.onkeydown
          (fn [e]
            (when @*kb-active?
              (let [current @*slide
                    idx     (condp contains? (.-keyCode e)
                              #{8 37 38 33 80}  (dec current) ;; backspace, left, up, pageup, p
                              #{13 32 39 40 34} (inc current) ;; enter, space, right, down, pagedown
                              #{36}             0             ;; home
                              #{35}             max-idx       ;; end
                              nil)]
                (when idx
                  (let [idx (-> idx (max 0) (min max-idx))]
                    (when (not= current idx)
                      (reset! *slide idx)))
                  (.preventDefault e)))))))
      state)
    :will-unmount
    (fn [state]
      (set! js/document.onkeydown nil)
      state) })


(rum/defc slides < rum/reactive, listen-keyboard [slides *slide]
  [:.slides
    (core/slide (nth slides (rum/react *slide) nil))])


(def set-starting-slide
  { :will-mount
    (fn [state] 
      (let [[*deck] (:rum/args state)]
        (swap! *deck assoc :presenter-slide 0))
      state)
    :will-unmount
    (fn [state] 
      (let [[*deck] (:rum/args state)]
        (swap! *deck dissoc :presenter-slide))
      state) })


(defn- send-question! [text *deck]
  (let [qid (u/ssid "q-")
        q   { :question/id  qid
              :deck/id      (:deck/id @*deck)
              :user/id      core/user-id
              :question/text text
              :question/created-at (js/Date.) }]
    (swap! *deck update :deck/questions assoc qid q)))


(defn cmp-q [q1 q2]
  (let [v1 (count (:question/upvoters q1))
        v2 (count (:question/upvoters q2))
        t1 (:question/created-at q1)
        t2 (:question/created-at q2)]
    (cond
      (< v1 v2) 1
      (> v1 v2) -1
      :else (compare t2 t1))))


(rum/defcs questions < (rum/local true ::open) [{*open? ::open} *deck]
  [:.questions
    [:.q-icon { :on-click (fn [e] (swap! *open? not)) }
      [:.q-icon-inner "?"]]
    (when @*open?
      [:.q-popup
        [:.q-popup-inner
          [:.q-questions
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
                [:.q-t]])]
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


(rum/defc present-page < rum/reactive
                         set-starting-slide
  [*deck]
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
        (slides (core/->slides-only content) (rum/cursor *deck [:presenter-slide])))]))


(rum/defc spectate-page < rum/reactive
  [*deck]
  (let [deck   (rum/react *deck)
        width  (rum/react core/*window-width)
        height (rum/react core/*window-height)
        max-w  (* height core/aspect)
        w      (min width max-w)
        h      (/ w core/aspect)]
    [:.page_deck-present
      { :style { :padding-top  (-> (- height h) (/ 2) (str "px"))
                 :padding-left (-> (- width w) (/ 2) (str "px"))
                 :font-size    (str (u/width->font-size w false) "px")
                 :height       height} }
      (questions *deck)
      (when-let [content (:deck/content deck)]
        [:.slides
         { :class (:deck/theme deck "default") }
         (let [slides (core/->slides-only content)
               slide (nth slides (:presenter-slide deck) nil)]
           (core/slide slide))])]))
