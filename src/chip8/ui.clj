(ns chip8.ui
  (:require [lanterna.screen :as s]
            [chip8.cpu :as cpu]
            [mount.core :refer [defstate]]))

(defstate screen
  :start (let [the-screen (s/get-screen :swing {:cols 256 :rows 32 :font "Times New Roman"})]
           (s/start the-screen)
           the-screen)
  :stop (s/stop screen))

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
        keyup (aget cpu/KEYUP 0)
        keydown (aget cpu/KEYDOWN 0)]
    (when (contains? #{\1 \2 \3 \4 \q \w \e \r \a \s \d \f \z \x \c \v null-char} character)
      (cond
        ;; 000
        ;; if everything is null, do nothing
        (= character keyup keydown null-char) nil
        ;; 001
        ;; key was released
        (and (= character keyup null-char)
             (not= keydown null-char)) (do
                                         (println "Key down: " keydown)
                                         (aset-char cpu/KEYDOWN 0 null-char)
                                         (aset-char cpu/KEYUP 0 keydown))
        ;; 010
        ;; key up event finished
        (and (= character keydown null-char)
             (not= keyup null-char)) (do
                                       (println "Key up: " keyup)
                                       (aset-char cpu/KEYUP 0 null-char))
        ;; 011
        ;; key was released while keyup event finished
        (and (= character null-char)
             (not= keydown null-char)
             (not= keyup null-char)) (do
                                       (println "Key up: " keyup " Key down: " keydown)
                                       (aset-char cpu/KEYDOWN 0 null-char)
                                       (aset-char cpu/KEYUP 0 keydown))
        ;; 100
        ;; key pressed
        (and (not= character null-char)
             (= keyup keydown null-char)) (do
                                            (println "Key pressed: " character)
                                            (aset-char cpu/KEYDOWN 0 character))
        ;; 101
        ;; key pressed while keydown event finished
        (and (= keyup null-char)
             (not= character null-char)
             (not= keydown null-char)) (do
                                         (println "Key pressed: " character " Key down: " keydown)
                                         (aset-char cpu/KEYUP 0 keydown)
                                         (aset-char cpu/KEYDOWN 0 character))
        ;; 110
        ;; key pressed while keyup event finished
        (and (= keydown null-char)
             (not= character null-char)
             (not= keyup null-char)) (do
                                       (println "Key pressed: " character " Key up: " keyup)
                                       (aset-char cpu/KEYDOWN 0 character)
                                       (aset-char cpu/KEYUP 0 null-char))
        ;; 111
        ;; key pressed while keydown and keyup events finished
        (and (not= character null-char)
             (not= keyup null-char)
             (not= keydown null-char)) (do
                                         (println "Key pressed: " character " Key up: " keyup " Key down: " keydown)
                                         (aset-char cpu/KEYDOWN 0 character)
                                         (aset-char cpu/KEYUP 0 keydown))))))
