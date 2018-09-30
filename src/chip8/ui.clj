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

(defn read-keyboard
  [key-event]
  (let [event (.getID key-event)
        char (.getKeyChar key-event)
        null-char java.lang.Character/MIN_VALUE]
    (when (= event java.awt.event.KeyEvent/KEY_PRESSED)
      (when (contains? #{\1 \2 \3 \4 \q \w \e \r \a \s \d \f \z \x \c \v null-char} char)
        (aset-char cpu/KEYDOWN 0 char)))
    (when (= event java.awt.event.KeyEvent/KEY_RELEASED)
      (when (contains? #{\1 \2 \3 \4 \q \w \e \r \a \s \d \f \z \x \c \v null-char} char)
        (aset-char cpu/KEYDOWN 0 null-char))))
  #_(let [character (or (s/get-key screen) java.lang.Character/MIN_VALUE)
        null-char java.lang.Character/MIN_VALUE
        keydown (aget cpu/KEYDOWN 0)]
    (when (contains? #{\1 \2 \3 \4 \q \w \e \r \a \s \d \f \z \x \c \v null-char} character)
      (aset-char cpu/KEYDOWN 0 character))))

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
                                                 (.drawImage g img 0 0 nil))))
    (seesaw/config! frm :content canvas)
    (-> frm seesaw/pack! seesaw/show!)))

(defstate screen :start (window) :stop (.dispose screen))

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
                         [] (into [] sprite-array)))
            sub-image (.getSubimage img 0 (* y 8) (* 64 8) 8)
            sub-image-g (.getGraphics sub-image)]
        (loop [x 0]
          (when (<= x 63)
            (let [screen-pixel (if (= (.getRGB sub-image (* x 8) 0) 0) 0 1)
                  frame-pixel (nth row x)]
              (graphics/draw sub-image-g
                             (graphics/rect (* x 8) 0 8 8)
                             (graphics/style :background (if (= (nth row x) 1) :grey :black)))
              (.drawImage sub-image-g sub-image 0 0 nil))
            (recur (inc x)))))
      (recur (inc y))))
  (.repaint screen))
