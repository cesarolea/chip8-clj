(ns chip8.core
  (:require [clojure.java.io :as io]
            [chip8.cpu :as cpu]
            [chip8.ui :as ui]
            [clojure.core.async :refer [go-loop close!]]
            [mount.core :refer [defstate start stop]])
  (:import [org.apache.commons.io IOUtils])
  (:gen-class))

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
             (Thread/sleep 1851)
             (recur)))
  :stop (close! cpu-clock))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (cpu/reset)
  (ui/start))
