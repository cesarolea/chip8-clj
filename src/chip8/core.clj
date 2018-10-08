(ns chip8.core
  (:require [clojure.java.io :as io]
            [chip8
             [cpu :as cpu]
             [ui :as ui]
             [sound :as sound]
             [options :as options]]
            [mount.core :refer [defstate start stop]])
  (:import [org.apache.commons.io IOUtils])
  (:gen-class))

(defonce sound-future (atom nil))

(defn read-rom-file
  "Reads a rom file from path and loads ROM into memory"
  [path]
  (cpu/load-rom (IOUtils/toByteArray (io/input-stream path))))

(defstate cpu-clock
  :start (future (while true
                   (when (cpu/running?)
                     (aset-char cpu/KEYDOWN 0 @ui/key)
                     (cpu/step)
                     (javax.swing.SwingUtilities/invokeLater #(.repaint ui/screen)))
                   (Thread/sleep (* (/ 1 (options/get-option :cpu-frequency)) 1000))))
  :stop (future-cancel cpu-clock))

(defstate sound-loop
  :start (future (while true
                   (when (and (cpu/running?)
                              (not (= (aget cpu/ST 0) 0)))
                     (when (or (and (future? @sound-future)
                                    (future-done? @sound-future))
                               (not (future? @sound-future)))
                       (reset! sound-future (future (sound/play (options/get-option :note)
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
