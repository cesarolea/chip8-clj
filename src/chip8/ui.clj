(ns chip8.ui
  (:require [lanterna.screen :as s]
            [chip8.cpu :as cpu]
            [mount.core :refer [defstate]])
  (:import [java.awt Graphics Color Dimension]
           [java.awt.event KeyListener KeyEvent]
           [java.awt.image BufferedImage]
           [javax.swing JPanel JFrame SwingUtilities]))

(defn render [^Graphics g ^long width ^long height])

(defn get-image [^long width ^long height]
  (let [image (BufferedImage. width height BufferedImage/TYPE_INT_RGB)]
    (render (.createGraphics image) width height)
    image))

(defn new-drawer []
  (proxy [JPanel] []
    (paint [^Graphics graphics-context]
      (let [^int width (proxy-super getWidth)
            ^int height (proxy-super getHeight)]
        (.drawImage graphics-context (get-image width height) 0 0 nil)))))

(defstate screen
  :start (let [the-screen (s/get-screen :swing {:cols 256 :rows 32 :font "Times New Roman"})]
           (s/start the-screen)
           the-screen)
  :stop (s/stop screen))

(defn window []
  (let [^JPanel drawing-obj (new-drawer)
        ^JFrame frame (JFrame. "clj-chip8")
        closer (proxy [KeyListener] []
                 (keyPressed [^KeyEvent e] (when (= (.getKeyChar e) \p) (.dispose frame)))
                 (keyReleased [e])
                 (keyTyped [e]))]
    (.setPreferredSize drawing-obj (Dimension. 64 32))
    (.add (.getContentPane frame) drawing-obj)))

(defn start [] (s/start screen))

(defn stop [] (s/stop screen))

(defn clear [] (s/clear screen) (s/redraw screen))

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
