(ns chip8.core
  (:require [clojure.java.io :as io]
            [chip8.cpu :as cpu]
            [chip8.ui :as ui]
            [chip8.sound :as sound]
            [clojure.core.async :refer [go-loop close!]]
            [mount.core :refer [defstate start stop]])
  (:import [org.apache.commons.io IOUtils])
  (:gen-class))

(defonce sound-future (atom nil))

(defn read-rom-file
  "Reads a rom file from path and loads ROM into memory"
  [path]
  (cpu/load-rom (IOUtils/toByteArray (io/input-stream path))))

(defstate cpu-clock
  :start (go-loop []
           (do
             (when (cpu/running?)
               (cpu/step)
               (ui/draw-screen cpu/framebuffer))
             (Thread/sleep (/ 1 540))
             (recur)))
  :stop (close! cpu-clock))

(defstate sound-loop
  :start (go-loop []
           (do
             (when (and (cpu/running?)
                        (not (= (aget cpu/ST 0) 0)))
               (reset! sound-future (future (sound/play 60 (Long/MAX_VALUE))))
               (cpu/dec-reg cpu/ST))
             (when (or (not (cpu/running?))
                       (= (aget cpu/ST 0) 0))
               (when @sound-future
                 (reset! sound-future (future-cancel @sound-future))))
             (Thread/sleep (/ 1 60))
             (recur)))
  :stop (close! sound-loop))

(defstate delay-loop
  :start (go-loop []
           (do
             (when (and (cpu/running?)
                        (not (= (aget cpu/DT 0) 0)))
               (cpu/dec-reg cpu/DT))
             (Thread/sleep (/ 1 60))
             (recur)))
  :stop (close! delay-loop))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (cpu/reset)
  (start)
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. #(stop))))
