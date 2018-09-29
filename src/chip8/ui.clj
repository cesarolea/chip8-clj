(ns chip8.ui
  (:require [lanterna.screen :as s]
            [chip8.cpu :as cpu]
            [mount.core :refer [defstate]]
            [seesaw.core :as seesaw]
            [seesaw.graphics :as graphics]
            [seesaw.color :refer [to-color]])
  (:import [java.awt.image BufferedImage]
           [java.awt.Color]))

(defstate screen
  :start (let [the-screen (s/get-screen :swing {:cols 256 :rows 32 :font "Times New Roman"})]
           (s/start the-screen)
           the-screen)
  :stop (s/stop screen))

(defn start [] (s/start screen))

(defn stop [] (s/stop screen))

(defn clear [] (s/clear screen) (s/redraw screen))

(defn window []
  (let [frm (seesaw/frame :title "clj-chip8" :resizable? false :on-close :dispose)
        canvas (seesaw/canvas)
        img (graphics/buffered-image 64 32)
        g2d (.getGraphics img)]
    (seesaw/native!)
    (graphics/anti-alias g2d)
    (seesaw/config! canvas :size [(* 64 8) :by (* 32 8)])
    (seesaw/config! canvas :paint (fn [c g] (try (graphics/draw g (graphics/rect 0 0
                                                                                 (seesaw/width c)
                                                                                 (seesaw/height c))
                                                                (graphics/style :background :black))
                                                 (.drawImage g img 0 0 nil))))
    (seesaw/config! frm :content canvas)
    (-> frm seesaw/pack! seesaw/show!)))

(defn draw-screen
  [framebuffer]
  ;; the framebuffer is an array of byte arrays. Each byte array has 8 bytes, meaning a single
  ;; element of the framebuffer array has a full row
  (loop [y 0]
    (when (<= y 31)
      (let [sprite-array (aget framebuffer y)
            row (flatten
                 (reduce (fn [acc itm]
                           (conj acc (into [] (cpu/bits itm 8))))
                         [] (into [] sprite-array)))]
        (loop [x 0]
          (when (<= x 63)
            (when (= 1 (nth row x)))
            (doseq [x-offset (range 0 4)]
              (s/put-string screen (+ (* x 4) x-offset) y " " {:bg (if (= (nth row x) 1) :white :black)}))
            (recur (inc x)))))
      (recur (inc y))))
  (s/redraw screen))

(defn read-keyboard
  []
  (let [character (or (s/get-key screen) java.lang.Character/MIN_VALUE)
        null-char java.lang.Character/MIN_VALUE
        keydown (aget cpu/KEYDOWN 0)]
    (when (contains? #{\1 \2 \3 \4 \q \w \e \r \a \s \d \f \z \x \c \v null-char} character)
      (aset-char cpu/KEYDOWN 0 character))))
