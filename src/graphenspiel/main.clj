(ns graphenspiel.main
  (:require [graphenspiel.drawing :as drawing]
            [graphenspiel.core :as core])
  (:gen-class))

; let's just get a minimal main so we can make a demo jar
(defn -main
  [& args]
  (drawing/start)
  (core/sim-loop))

