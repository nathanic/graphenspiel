(ns nodebeat.math
  (:import java.lang.Math))

; where did these come from before?  quil?
; (def ^:private sqrt java.lang.Math/sqrt)
; (def ^:private pow java.lang.Math/pow)

(defn distance 
  "calculate the euclidian distance (l2 norm) between two points"
  [[x1 y1] [x2 y2]]
  (Math/hypot (- x2 x1) (- y2 y1))
  #_(sqrt (+ (pow (- x2 x1) 2)
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

(comment
  (distance [0 0] [1 1])
  (linterp [0 0] [1 1] (/ (Math/sqrt 2) 2))
  )
