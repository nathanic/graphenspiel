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
  ; iterate that shit
    ; update the state in some reasonable way
  ; implement node creation
    ; probably just click to create nodes to start out 


; window dimensions
(def cx 400)
(def cy 400)

(defrecord Node
  [pos
   ;color
   ;rate
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
  (let [[x1 y1]  (:pos n1)
        [x2 y2]  (:pos n2)] 
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

(defn draw []
  ; edge
  (stroke 128 128 128)
  (line 100 100 300 300)

  ; nodes
  (fill 128 128 255)
  (stroke 0 0 0) 
  (ellipse 100 100 50 50)
  (ellipse 300 300 50 50)

  ; blip
  (let [n1   (Node. [100 100]) 
        n2   (Node. [300 300]) 
        blip (Blip. n1 n2 50)] 
    (draw-blip blip))
  )
; how do i stop it from leaving residual artifacts from the blips?

(defn setup []
  (frame-rate 24)
  (smooth)
  (background 180)
  (stroke 0)
  (stroke-weight 5)
  (fill 255 25)
  (let [n1   (Node. [100 100]) 
        n2   (Node. [300 300]) 
        blip (Blip. n1 n2 200)] 
    (set-state! {:nodes (atom [n1 n2]) 
                 :blips (atom [blip])})
    ))

(defsketch simple-circle
  :title "circles and stuff"
  :setup setup
  :draw draw
  :size [cx cy]
  :keep-on-top true)


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
