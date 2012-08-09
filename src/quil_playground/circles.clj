(ns circles 
  (:use quil.core
        [quil.helpers.seqs :only [seq->stream range-incl]]))

(def cx 400)
(def cy 400)

(deftype Blip [ origin  ; origin node
                dest    ; destination node
                pos     ; scalar position between nodes
               ])

; need a function (Blip -> [x y])
(defn draw-blip
  []
  )

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
  (no-stroke)
  (fill 250 200 30)
  (ellipse 200 200 10 10)
  )

(defn setup []
  (frame-rate 24)
  (smooth)
  (background 180)
  (stroke 0)
  (stroke-weight 5)
  (fill 255 25)
  )

(defsketch simple-circle
  :title "just some circles"
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

  ; steps:
  ; draw a circle
  ; draw two circles with a line between them
  ; draw some kind of blip along the line
  )

; it would be cool to bind a vim key to eval a sexpr
; at a specific bookmark
; :nnoremap <f11> 'e,e
