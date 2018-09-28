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
               (ui/read-keyboard)
               (ui/draw-screen cpu/framebuffer))
             (Thread/sleep (/ (/ 1 720) 1000))
             (recur)))
  :stop (close! cpu-clock))

(defstate sound-loop
  :start (go-loop []
           (do
             (when (and (cpu/running?)
                        (not (= (aget cpu/ST 0) 0)))
               (when (or (and (future? @sound-future)
                              (future-done? @sound-future))
                         (not (future? @sound-future)))
                 (reset! sound-future (future (sound/play 60 (* (cpu/byte->ubyte (aget cpu/ST 0))
                                                                (/ 1 60)
                                                                1000)))))
               (cpu/dec-reg cpu/ST))
             (Thread/sleep (/ (/ 1 60) 1000))
             (recur)))
  :stop (close! sound-loop))

(defstate delay-loop
  :start (go-loop []
           (do
             (when (and (cpu/running?)
                        (not (= (aget cpu/DT 0) 0)))
               (cpu/dec-reg cpu/DT))
             (Thread/sleep (/ (/ 1 60) 1000))
             (recur)))
  :stop (close! delay-loop))

(defn -main
  [& args]
  (cpu/reset)
  (start)
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. #(stop))))
