(ns chip8.sound
  (:require [mount.core :refer [defstate]])
  (:import [javax.sound.midi MidiSystem Synthesizer]))

(defn- play-note [synth channel note]
  (let [n (:note note 60)
        v (:velocity note 127)
        d (:duration note 1000)]
    (. channel noteOn n v)
    (Thread/sleep d)
    (. channel noteOff n)))

(defn play-notes
  "Plays collection notes using the default system Synthesizer.
  Elements in notes should be hash-maps, with optional keys :note,
  :velocity, and :duration (ms)"
  [notes]
  (with-open [synth (doto (MidiSystem/getSynthesizer) .open)]
    (let [channel (aget (.getChannels #^Synthesizer synth) 0)]
      (loop [remaining notes]
        (when-not (empty? remaining)
          (play-note synth channel (first remaining))
          (recur (rest remaining)))))))

(defn play
  [note duration]
  (with-open [synth (doto (MidiSystem/getSynthesizer) .open)]
    (let [channel (aget (.getChannels #^Synthesizer synth) 0)]
      (play-note synth channel {:note note :duration duration}))))
