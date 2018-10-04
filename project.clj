(defproject chip8 "0.1.1"
  :description "A CHIP-8 interpreter in Clojure"
  :url "https://github.com/cesarolea/chip8-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [commons-io/commons-io "2.6"]
                 [mount "0.1.13"]
                 [seesaw "1.5.0"]]
  :main ^:skip-aot chip8.core
  :target-path "target/%s"
  :pedantic? true
  :profiles {:uberjar {:aot :all}})
