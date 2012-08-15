(ns circles 
  (:use quil.core
        [quil.helpers.seqs :only [seq->stream range-incl]]))

; the eventual goal is to make something like nodebeat
; that is, a graph of sundry nodes that generate music.
; steps:
  ; draw a circle
    ; check
  ; draw two circles with a line between them
    ; check
  ; draw some kind of blip along the line
    ; check
  ; parameterize that shit
    ; use the state stuff
    ; check
  ; iterate that shit
    ; update the state in some reasonable way
  ; implement node creation
    ; probably just click to create nodes to start out 


; window dimensions
(def cx 400)
(def cy 400)

(def node-radius 40)

(comment
  so this is really a digraph
  and that should be reflected in my data structures
  need to distinguish sources from sinks...

  maybe two kinds of Nodes
    sources and sinks
    sources have pointers to their sinks
    but then what of blips?

  source nodes generate pulses periodically.
  sink nodes receive any pulses and respond with an animation and sound,
  pulses travel along graph edges at some speed 
    uniform or variable? 
  the nature of which can depend upon both the type of the source and that of
  the sink.
    could probably dispatch a multimethod on that

  it would also be interesting to also have transceiver nodes that receive and
  retransmit a modified pattern...
    a rate divider
    rate multiplier
    signal gate
    pulse stretcher?
    pulse shrinker?
  are my pulses discrete lengths or impulses? 

  on each tick of the simulation
    advance all pulses
      (+ position (* pulse-rate delta-t))
      could be fancy and schedule the handler for interpolated arrival time
      actually, could do the whole thing that way
      more declarative than imperative
    respond to pulse arrivals at nodes
      remove pulse
      initiate next pulse
      start a synth etc.
      start an animation

  does processing/quil make it easy to separate the simulation update
  from the draw loop?
  i guess i could do that anyway with another thread and some atoms/refs

  it's funny that i'm reaching for OO
  but OO has always been good for simulation
)


(defrecord Node
  [pos
   ;sinks  ; list of Nodes connected as sinks
   ;color
   ;rate   ; rate of pulses in Hz
   ;kind
   ]
  )

(defrecord Blip 
  [origin  ; origin node
   dest    ; destination node
   pos     ; scalar position along edge between nodes
  ])

; good ol' euclid
(defn node-dist 
  [n1 n2]
  (let [[x1 y1] (:pos n1)
        [x2 y2] (:pos n2)] 
    (sqrt (+ (pow (- x2 x1) 2)
             (pow (- y2 y1) 2)))))

; linear interpolation
(defn blip-pos
  [blip]
  (let [[ox oy] (:pos (:origin blip))
        [dx dy] (:pos (:dest blip))
        ratio   (/ (:pos blip) 
                   (node-dist (:origin blip) (:dest blip))) ]
    [(+ ox (* ratio (- dx ox))) 
     (+ oy (* ratio (- dy oy)))]))

(defn draw-blip
  [blip]
  (let [[x y] (blip-pos blip)]
    #_(println "drawing a blip at " x y)
    (no-stroke)
    #_(fill 250 200 30)
    (fill 0 200 30)
    (ellipse x y 10 10)))


(defn draw-node
  [node]
  (let [[x y] (:pos node)]
    (fill 128 128 255)
    (stroke 0 0 0) 
    (ellipse x y node-radius node-radius)))


; (set (map (comp sort (juxt :origin :dest)) nodes))
; really need to clean up the blips vs edges confusion
(defn all-edges [blips]
  (set (map (fn [n] 
              (sort [(:pos (:origin n)) 
                     (:pos (:dest n))])) 
            blips)))

(defn draw-edges [blips]
  (stroke 128 128 128)
  (doseq [[[x0 y0] 
           [x1 y1]] (all-edges blips)]
    #_(println "drawing edge" x0 y0 x1 y1)
    (line x0 y0 x1 y1)))

(defn draw []
  (draw-edges @(state :blips))
  (doall (map draw-node @(state :nodes)))
  (doall (map draw-blip @(state :blips)))
)
; how do i stop it from leaving residual artifacts from the blips?


; arguments to set-state!, defined here to make things
; easer for interactive eval
(def initial-state
  (let [n1 (Node. [100 100]) 
        n2 (Node. [300 300]) 
        n3 (Node. [125 250]) 
        b1 (Blip. n1 n2 200) ; blips are complected with edges
        b2 (Blip. n1 n3 50)
        b3 (Blip. n2 n3 100) ] 
    {:nodes (atom [n1 n2 n3]) 
     :blips (atom [b1 b2 b3])}
    ))


(comment
  (def nodes (:nodes initial-state))
  (def blips (:blips initial-state))
  (prn @nodes)
  (prn @blips)
  (count (all-edges @blips))
  (doseq [q  (all-edges @blips)] (println "q:" q))
  )

(defn setup []
  (frame-rate 24)
  (smooth)
  (background 180)
  (stroke 0)
  (stroke-weight 5)
  (fill 255 25)
  (apply set-state! (apply concat initial-state))
  )

(comment

  (use 'clojure.repl)
  (use 'clojure.pprint)

  (defn calc-next-state 
    [nodes blips]
    [nodes (map #(update-in % [:pos] inc) blips)] 
    )
  (calc-next-state [] [(Blip. nil nil 1)])

  (pprint initial-state)
  (let [[n' b'] (calc-next-state @(:nodes initial-state) 
                                 @(:blips initial-state))]
    (reset! (:nodes initial-state) n')
    (reset! (:blips initial-state) b')
    )

(defsketch some-nodes 
  :title "graphs and stuff"
  :setup setup
  :draw draw
  :size [cx cy]
  :keep-on-top true)
  
)


(comment
  (use 'clojure.repl)
  (doc stroke)
  (doc background)
  (doc stroke-weight)
  (doc fill)


  (def n1   (Node. [100 100]))
  (def n2   (Node. [300 300])) 
  (def blip (Blip. n1 n2 100)) 
  (node-dist n1 n2)
  (blip-pos blip)

  (prn blip)
  (update-in blip [:pos] inc)

  (def ratio (+ 100 (* 200 (/ 100 (node-dist n1 n2)))))
  (def ratio (/ (:pos blip) 
               (node-dist (:origin blip) (:dest blip))))
  (:origin blip)
  (:dest blip)
  (def ox (first (:pos (:origin blip))))
  (def oy (nth (:pos (:origin blip)) 1))
  (def dx (first (:pos (:dest blip))))
  (def dy (nth (:pos (:dest blip)) 1))


  )

; it would be cool to bind a vim key to eval a sexpr
; at a specific bookmark
; :nnoremap <f11> 'e,e
