(ns deckatron.pages.present
  (:require
    [clojure.string :as str]
    [rum.core :as rum]
    [deckatron.core :as core]
    [deckatron.util :as u]
    [deckatron.parser :as parser]))


(enable-console-print!)


(def listen-keyboard
  { :did-mount
    (fn [state]
      (let [[slides *slide] (:rum/args state)
            max-idx         (dec (count slides))]
        (set! js/document.onkeydown
          (fn [e]
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
                (.preventDefault e))))))
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
      { :style { :padding-top  (-> (- height h) (/ 2) (str "px"))
                 :padding-left (-> (- width w) (/ 2) (str "px"))
                 :font-size    (str (u/width->font-size w false) "px")
                 :height       height} }
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
      (when-let [content (:deck/content deck)]
        [:.slides
         (let [slides (core/->slides-only content)
               slide (nth slides (:presenter-slide deck) nil)]
           (core/slide slide))])]))
