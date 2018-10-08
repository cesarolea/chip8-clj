(ns chip8.options)

(defonce options (atom {:cpu-frequency 720 :scaling 8 :note 60}))

(defn set-option [key val]
  (swap! options update-in [key] (constantly val)))

(defn get-option [key]
  (key @options))
