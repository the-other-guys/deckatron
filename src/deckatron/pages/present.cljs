(ns deckatron.pages.present
  (:require
    [clojure.string :as str]
    [rum.core :as rum]
    [deckatron.core :as core]
    [deckatron.util :as u]
    [deckatron.parser :as parser]))


(enable-console-print!)


(defonce *slide (atom 0))


(defn listen-keyboard [on-keydown]
  { :did-mount
    (fn [state]
      (.addEventListener js/document "keydown" on-keydown)
      state)
    :will-unmount
    (fn [state]
      (.removeEventListener js/document "keydown" on-keydown)
      state) })


(defn handle-keyboard [e]
  (when (condp contains? (.-keyCode e)
          #{8 37 38 33 80}  (swap! *slide dec)    ;; backspace, left, up, pageup, p
          #{13 32 39 40 34} (swap! *slide inc)    ;; enter, space, right, down, pagedown
          #{36}             (reset! *slide 0)     ;; home
          #{35}             (reset! *slide 10000) ;; end
          nil)
    (.preventDefault e)))


(rum/defc slides  < rum/reactive
                    (listen-keyboard handle-keyboard)
  [slides]
  (let [idx (-> (rum/react *slide)
                (max 0)
                (min (dec (count slides))))]
    (when (not= (rum/react *slide) idx)
      (reset! *slide idx))
    [:.slides
      (core/slide (nth slides idx))]))


(rum/defc present-page < rum/reactive
  [*deck socket]
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
        (slides (core/slides content)))]))

