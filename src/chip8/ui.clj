(ns chip8.ui
  (:require [chip8
             [cpu :as cpu]
             [options :as options]]
            [mount.core :refer [defstate]]
            [seesaw
             [core :as seesaw]
             [graphics :as graphics]
             [color :refer [to-color]]
             [dev :refer [show-events]]])
  (:import [java.awt.image BufferedImage]
           [java.awt.Color]))

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
  (let [img (graphics/buffered-image (* 64 (options/get-option :scaling))
                                     (* 32 (options/get-option :scaling)))
        frm (seesaw/frame :title "chip8-clj" :resizable? false :on-close :dispose
                          :listen [:key-pressed read-keyboard :key-released read-keyboard])
        canvas (seesaw/canvas)
        g2d (.getGraphics img)]
    (seesaw/native!)
    (graphics/anti-alias g2d)
    (seesaw/config! canvas :size [(* 64 (options/get-option :scaling)) :by (* 32 (options/get-option :scaling))])
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

                                              ;; gets the subimage of the current row only
                                              sub-image (.getSubimage img
                                                                      0                                 ;; x coord
                                                                      (* y (options/get-option :scaling))  ;; y coord
                                                                      (* 64 (options/get-option :scaling)) ;; witdth
                                                                      (options/get-option :scaling))       ;; height
                                              sub-image-g (.getGraphics sub-image)]

                                          (graphics/draw sub-image-g
                                                                 (graphics/rect 0 0
                                                                                (* 64 (options/get-option :scaling))
                                                                                (options/get-option :scaling))
                                                                 (graphics/style :background :black))
                                          (.drawImage sub-image-g sub-image 0 0 nil)

                                          (loop [x 0]
                                            (when (<= x 63)
                                              (let [frame-pixel (nth row x)]
                                                (when (= 1 frame-pixel)
                                                  (graphics/draw sub-image-g
                                                                 (graphics/rect (* x (options/get-option :scaling)) 0
                                                                                (options/get-option :scaling)
                                                                                (options/get-option :scaling))
                                                                 (graphics/style :background (if (= 1 frame-pixel) :grey :black)))
                                                  (.drawImage sub-image-g sub-image 0 0 nil)))
                                              (recur (inc x)))))
                                        (recur (inc y))))))
    (seesaw/config! frm :content canvas)
    (-> frm seesaw/pack! seesaw/show!)))

(defstate screen :start (window) :stop (.dispose screen))
