(ns circles 
  (:use quil.core
        [quil.helpers.seqs :only [seq->stream range-incl]])
  (:import java.lang.Thread)
  )

; the eventual goal is to make something like nodebeat
; that is, a graph of sundry nodes that generate musical sounds.


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

  
  does processing/quil offer me enough UI stuff?
  should i be doing this with seesaw instead?
  
  TODO
    differentiate node types
      octave source
      percussion source
      various sink types
        synths
        samplers
          load arbitrary files
        percussion
          evaluate synths and samples for these
      pulse rate modifiers
    animate nodes in response to pulses
    play sounds in response to pulses
    create nodes with mouse
    drag/drop nodes
    node motion

)


(defrecord Node
  [pos
   ;sinks  ; list of Nodes connected as sinks
           ; what about a list of connected Edges?
   ;color
   ;rate   ; rate of pulses in Hz
   ;kind
   ]
  )
; maybe Edges are separate from blips
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
  (background 180)
  (stroke 0)
  (stroke-weight 5)
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
        n4 (Node. [220 350]) 
        b1 (Blip. n1 n2 200) ; blips are complected with edges
        b2 (Blip. n1 n3 50)
        b3 (Blip. n3 n2 100)
        b4 (Blip. n3 n4 10)  
        b5 (Blip. n2 n4 10)  
        ] 
    {:nodes (atom [n1 n2 n3 n4]) 
     :blips (atom [b1 b2 b3 b4 b5])}
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



(comment ; code incubator

  (do  
    (use 'clojure.repl)
    (use 'clojure.pprint)

    (def pos-increment 5)
    (defn update-blip-pos [blip]
      (let [dist (node-dist (:origin blip) (:dest blip))] 
        (cond
          ; reset to zero if we hit the limit
          (>= (:pos blip) dist)
          (update-in blip [:pos] (constantly 0))
          ; todo: trigger some kind of event handler based on origin & dest types

          :otherwise
          (update-in blip [:pos] + pos-increment)
          )))

    ; state calculation should be a pure function
    ; i'm also tempted to just keep the state as one value
    ; in a single atom
    (defn calc-next-state 
      [nodes blips]
      [nodes 
      (map update-blip-pos blips)]) 
    
  )

  ; should i use defmulti to set up behaviors?
  ; or records & a protocol?
  ; i guess i already have records above...
  ; but i could probably still use defmulti in conjunction 
  (defmulti react-to-hit ...)
  (defmethod react-to-hit :Node ...)

  (pprint initial-state)

  ; let's just try a shitty little update loop to animate the blips
  ; should eventually have this kicked off by (setup)
  ; need to stash it in state
  ; hopefully there is some kind of teardown event...
  ; otherwise i need to make arrange for it to stop some other way
  (def animation-thread 
    (future              
      (loop []
        (let [[n' b'] (calc-next-state @(:nodes initial-state) 
                                       @(:blips initial-state))]
          (reset! (:nodes initial-state) n')
          (reset! (:blips initial-state) b')
          (Thread/sleep 50)
          (recur)))))
  ; this is an abuse of futures but damn handy
  (future-cancel animation-thread)

  ; consider https://github.com/overtone/at-at/ for this


(defsketch some-nodes 
  :title "graphs and stuff"
  :setup setup
  :draw draw
  :size [cx cy]
  :keep-on-top true)
  
comment)


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
