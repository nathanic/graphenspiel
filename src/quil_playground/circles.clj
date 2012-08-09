(ns circles 
  (:use quil.core
        [quil.helpers.seqs :only [seq->stream range-incl]]))

(def cx 400)
(def cy 400)

(defn draw []
  (stroke 128 128 128)
  (line 100 100 300 300)

  (fill 128 128 255)
  (stroke 0 0 0) 
  (ellipse 100 100 50 50)
  (ellipse 300 300 50 50)
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
  :title "just a circle"
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
  )

; it would be cool to bind a vim key to eval a sexpr
; at a specific bookmark
; :nnoremap <f11> 'e,e
