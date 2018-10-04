(ns chip8.ui
  (:require [chip8.cpu :as cpu]
            [mount.core :refer [defstate]]
            [seesaw.core :as seesaw]
            [seesaw.graphics :as graphics]
            [seesaw.color :refer [to-color]]
            [seesaw.dev :refer [show-events]])
  (:import [java.awt.image BufferedImage]
           [java.awt.Color]))

(defonce img (graphics/buffered-image (* 64 8) (* 32 8)))
(defonce key (atom 0))

(defn read-keyboard
  "Keyboard event listener. On press sets the key register in the CPU. On release clears the key
  register."
  [key-event]
  (let [event (.getID key-event)
        char (.getKeyChar key-event)
        null-char java.lang.Character/MIN_VALUE]
    (when (contains? #{\1 \2 \3 \4 \q \w \e \r \a \s \d \f \z \x \c \v null-char} char)
      (condp = event
        java.awt.event.KeyEvent/KEY_PRESSED (reset! key char)
        java.awt.event.KeyEvent/KEY_RELEASED (reset! key null-char)
        true nil))))

(defn window []
  (let [frm (seesaw/frame :title "clj-chip8" :resizable? false :on-close :dispose
                          :listen [:key-pressed read-keyboard :key-released read-keyboard])
        canvas (seesaw/canvas)
        g2d (.getGraphics img)]
    (seesaw/native!)
    (graphics/anti-alias g2d)
    (seesaw/config! canvas :size [(* 64 8) :by (* 32 8)])
    (seesaw/config! canvas :paint (fn [c g] (try (graphics/draw g (graphics/rect 0 0
                                                                                 (seesaw/width c)
                                                                                 (seesaw/height c))
                                                                (graphics/style :background :black))
                                                 (.drawImage g img 0 0 nil)
                                                 (.repaint c))

                                    (loop [y 0]
                                      (when (<= y 31)
                                        (let [sprite-array (aget cpu/framebuffer y)
                                              row (flatten
                                                   (reduce (fn [acc itm]
                                                             (conj acc (into [] (cpu/bits itm 8))))
                                                           [] (into [] sprite-array)))
                                              sub-image (.getSubimage img 0 (* y 8) (* 64 8) 8)
                                              sub-image-g (.getGraphics sub-image)]
                                          (loop [x 0]
                                            (when (<= x 63)
                                              (let [frame-pixel (nth row x)]
                                                (when (= frame-pixel 1)
                                                  (graphics/draw sub-image-g
                                                                 (graphics/rect (* x 8) 0 8 8)
                                                                 (graphics/style :background :grey))
                                                  (.drawImage sub-image-g sub-image 0 0 nil)))
                                              (recur (inc x)))))
                                        (recur (inc y))))))
    (seesaw/config! frm :content canvas)
    (-> frm seesaw/pack! seesaw/show!)))

(defstate screen :start (window) :stop (.dispose screen))
