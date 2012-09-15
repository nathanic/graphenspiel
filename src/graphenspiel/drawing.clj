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

(defn draw-node* 
  "helper to draw a node of a given color"
  [node color]
  (stroke 0 0 0) 
  (apply fill color)
  (let [[x y] (:pos node)] 
    (ellipse x y node-radius node-radius)))

(defmulti draw-node (fn [state node] (:kind node)))

(defmethod draw-node :source
  [state node]
  (draw-node* node [255 0 0]))

(defmethod draw-node :sink
  [state node]
  (let [color (if (contains? node :reacting)
                [200 200 40]
                [128 128 255]) ] 
    (draw-node* node color)))

(defn- draw-nodes 
  [st]
  (stroke 0 0 0) 
  (doseq [node (vals (get-in st [:graph :nodes]))]
    (draw-node st node)))

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

; TODO: applet stuff like gtrak's elastic collision demo?
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


