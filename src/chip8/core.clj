(ns chip8.core
  (:require [clojure.java.io :as io]
            [chip8.cpu :as cpu]
            [chip8.ui :as ui]
            [chip8.sound :as sound]
            [mount.core :refer [defstate start stop]])
  (:import [org.apache.commons.io IOUtils])
  (:gen-class))

(defonce sound-future (atom nil))
(defonce options (atom {:cpu-frequency 720 :scaling 8 :note 60}))
(defonce cpu-frequency (atom 540))

(defn read-rom-file
  "Reads a rom file from path and loads ROM into memory"
  [path]
  (cpu/load-rom (IOUtils/toByteArray (io/input-stream path))))

(defstate render-loop
  :start (future (while true
                   (when (cpu/running?) (ui/draw-screen cpu/framebuffer))))
  :stop (future-cancel render-loop))

(defstate cpu-clock
  :start (future (while true
                   (when (cpu/running?)
                     (aset-char cpu/KEYDOWN 0 @ui/key)
                     (cpu/step))
                   (Thread/sleep (* (/ 1 (:cpu-frequency @options)) 1000))))
  :stop (future-cancel cpu-clock))

(defstate sound-loop
  :start (future (while true
                   (when (and (cpu/running?)
                              (not (= (aget cpu/ST 0) 0)))
                     (when (or (and (future? @sound-future)
                                    (future-done? @sound-future))
                               (not (future? @sound-future)))
                       (reset! sound-future (future (sound/play (:note @options)
                                                                (* (cpu/byte->ubyte (aget cpu/ST 0))
                                                                   (/ 1 60)
                                                                   1000)))))
                     (cpu/dec-reg cpu/ST))
                   (Thread/sleep (* (/ 1 60) 1000))))
  :stop (future-cancel sound-loop))

(defstate delay-loop
  :start (future (while true
                   (when (and (cpu/running?)
                              (not (= (aget cpu/DT 0) 0)))
                     (cpu/dec-reg cpu/DT))
                   (Thread/sleep (* (/ 1 60) 1000))))
  :stop (future-cancel delay-loop))

(defn -main
  [& args]
  (cpu/reset)
  (start)
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. #(stop))))
