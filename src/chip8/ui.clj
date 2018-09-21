(ns chip8.ui
  (:require [lanterna.screen :as s]
            [chip8.cpu :as cpu]))

(defonce screen (s/get-screen :swing {:cols 256 :rows 32 :font "Times New Roman"}))

(defn start [] (s/start screen))

(defn stop [] (s/stop screen))

(defn clear [] (s/clear screen) (s/redraw screen))

(defn- bits [n s]
  (take s
       (map
         (fn [i] (bit-and 0x01 i))
         (iterate
           (fn [i] (bit-shift-right i 1))
           n))))

(defn draw-screen
  [framebuffer]
  ;; the framebuffer is an array of byte arrays. Each byte array has 8 bytes, meaning a single
  ;; element of the framebuffer array has a full row
  (loop [y 0]
    (when (<= y 31)
      (let [sprite-array (aget framebuffer y)
            row (flatten
                 (reduce (fn [acc itm]
                           (conj acc (into [] (reverse (bits itm 8)))))
                         [] (into [] sprite-array)))]
        (loop [x 0]
          (when (<= x 63)
            (when (= 1 (nth row x)))
            (s/put-string screen x y " " {:bg (if (= (nth row x) 1) :white :black)})
            (recur (inc x)))))
      (recur (inc y))))
  (s/redraw screen))
