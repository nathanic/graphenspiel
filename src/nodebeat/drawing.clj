(ns nodebeat.drawing
  (:use nodebeat.core
        nodebeat.math
        quil.core))

; swanky helpers
(use '[clojure repl pprint])

; drawing config
(def cx 400) ; window dimensions
(def cy 400)
(def node-radius 50)
(def pulse-radius 10)

(defn- draw-edges
  [state]
  (stroke-weight 5)
  (stroke 128 128 128)
  (doseq [edge (get-in state [:graph :edges])]
    (->> (get-edge-nodes state edge)
      (map :pos)
      (apply line))))

(defn- draw-nodes 
  [state]
  (stroke 0 0 0) 
  (doseq [node (vals (get-in state [:graph :nodes]))
          :let [[x y] (:pos node)

                ; this color business is temporary
                col   (case (:kind node) 
                        :source [255   0   0] 
                        :sink   [128 128 255]) ]] 
    (apply fill col)
    (ellipse x y node-radius node-radius) ))

(defn- draw-pulses 
  [state]
  (no-stroke)
  (fill 0 200 30)
  (doseq [pulse (get-in state [:pulses])
          :let [[org dst] (get-edge-nodes state (:edge pulse))
                [x y]     (linterp (:pos org) (:pos dst) (:pos pulse))
                ]] 
    ; (println "ellipse" x y pulse-radius pulse-radius)
    (ellipse x y pulse-radius pulse-radius))) 


(defn- draw []
  (let [state @the-state] 
    (background 180)
    (draw-edges state)
    (draw-nodes state)
    (draw-pulses state)
    ))

(defn- setup []
  (frame-rate 24)
  (smooth)
  (background 180)
  (stroke 0)
  (stroke-weight 5)
  (fill 255 25)
  )

(defn start
  []
  (defsketch minimal-nodes 
             :title "minimal nodebeat recreation"
             :setup setup
             :draw draw
             :size [cx cy]
             :keep-on-top true))

(comment
  (start)
  )


