(ns minimal-nodebeat
  (:use quil.core
        [quil.helpers.seqs :only [seq->stream range-incl]])
  (:import java.lang.Thread))

; swanky helpers
(use '[clojure repl pprint])

; let's just set up a simple initial state and work with that for now
(def initial-state
  {:graph {:nodes {:src0 {:kind :source
                          :pos [50 50]
                          :pulse-interval 100
                          :created 0 ; beginning of time
                          }
                   :snk0 {:kind :sink
                          :pos [100 200]
                          }
                   }
           :edges [[:src0 :snk0]] ; list of pairs of node keys
           } 
   :pulses [{:edge [:src0 :snk0]
             :pos  50
             }]
   })

(def the-state (atom initial-state))

(defn get-edge-nodes 
  [state edge]
  [(get-in state [:graph :nodes (first edge)])
   (get-in state [:graph :nodes (second edge)])])

; drawing config
(def cx 400) ; window dimensions
(def cy 400)
(def node-radius 50)
(def pulse-radius 10)

(defn distance 
  "calculate the euclidian distance (l2 norm) between two points"
  [[x1 y1] [x2 y2]]
  (sqrt (+ (pow (- x2 x1) 2)
           (pow (- y2 y1) 2))))

; need a rendering function based on the-state
; also need my tick machinery
; which to do first?
; a simple drawing routine then
(defn linterp 
  "perform linear interpolation between two points
  given a scalar position along the line they form.
  returns a vector [x y]."
  [p1 p2 pos]
  (let [[x1 y1] p1
        [x2 y2] p2
        ratio   (/ pos (distance p1 p2)) ]
    [(+ x1 (* ratio (- x2 x1))) 
     (+ y1 (* ratio (- y2 y1)))]))

(defn draw-edges
  [state]
  (stroke-weight 5)
  (stroke 128 128 128)
  (doseq [edge (get-in state [:graph :edges])]
    (->> (get-edge-nodes state edge)
      (map :pos)
      (apply line))))

(defn draw-nodes 
  [state]
  ; (fill 128 128 255)
  (stroke 0 0 0) 
  (doseq [node (vals (get-in state [:graph :nodes]))
          :let [[x y] (:pos node)

                ; this color business is temporary
                col   (case (:kind node) 
                        :source [255   0   0] 
                        :sink   [128 128 255]) ]] 
    (apply fill col)
    (ellipse x y node-radius node-radius) ))

(defn draw-pulses 
  [state]
  (no-stroke)
  (fill 0 200 30)
  (doseq [pulse (get-in state [:pulses])
          :let [[org dst] (get-edge-nodes state (:edge pulse))
                [x y]     (linterp (:pos org) (:pos dst) (:pos pulse))
                ]] 
    ; (println "ellipse" x y pulse-radius pulse-radius)
    (ellipse x y pulse-radius pulse-radius))) 

(comment
  (doc dist)
  (let [[x y] (linterp [0 0] [100 100] (/ 141 2))]
    [x y])
  (dist 0 0 100 100)
  (distance [0 0] [100 100])
  (map :pos (get-edge-nodes initial-state [:src0 :snk0]))

  (def st initial-state)
  (def pulse (first (:pulses st)))
  (let [[org dst] (get-edge-nodes st (:edge pulse)) ]
     (println (:pos org) (:pos dst) (:pos pulse)))

  (doseq [pulse (get-in st [:pulses])
          :let [[org dst] (get-edge-nodes st (:edge pulse))
                [x y]     (linterp (:pos org) (:pos dst) (:pos pulse))
                ]] 
    (println "ellipse" x y pulse-radius pulse-radius)))

(defn draw []
  (let [state @the-state] 
    (background 180)
    (draw-edges state)
    (draw-nodes state)
    (draw-pulses state)
    ))

(defn setup []
  (frame-rate 24)
  (smooth)
  (background 180)
  (stroke 0)
  (stroke-weight 5)
  (fill 255 25)
  )

(comment
  (defsketch minimal-nodes 
             :title "minimal nodebeat recreation"
             :setup setup
             :draw draw
             :size [cx cy]
             :keep-on-top true))
