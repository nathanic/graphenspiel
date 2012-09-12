(ns graphenspiel.drawing
  (:use graphenspiel.core
        graphenspiel.math
        quil.core))

; swanky li'l helpers
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
    (->> (edge-nodes state edge)
      (map :pos)
      (apply line))))

(defn- draw-nodes 
  [state]
  (stroke 0 0 0) 
  (doseq [node (vals (get-in state [:graph :nodes]))
          :let [[x y] (:pos node)

                ; this color business is temporary
                ; will probably eventually go to a multimethod
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
          :let [[org dst] (edge-nodes state (:edge pulse))
                [x y]     (linterp (:pos org) (:pos dst) (:pos pulse)) ]] 
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
  (stroke 0)
  (stroke-weight 5)
  (fill 255 25)

  ; TODO: reference the state atom in this sketch instance's state?
  ; that way we could have multiple independent instances within one process 
  )

(defn start
  []
  (defsketch minimal-nodes 
             :title "Graphenspiel!"
             :setup setup
             :draw draw
             :size [cx cy]
             :keep-on-top true))

(comment
  (start)
  )


