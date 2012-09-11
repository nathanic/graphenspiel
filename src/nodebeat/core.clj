(ns nodebeat.core)

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

; the once piece of shared mutable state
(def the-state (atom initial-state))

(defn edge-nodes 
  "returns the pair of node records referenced by an edge"
  [state edge]
  [(get-in state [:graph :nodes (first edge)])
   (get-in state [:graph :nodes (second edge)])])


(comment
  (let [st @the-state] 
    (edge-nodes st (get-in st [:graph :edges 0])))
  )


; now to do some state-updating
; and entity ticking
(comment
  
  ; future, loop/recur, thread sleep, 
  ; reduce node ticks into the state 
  ; i guess also tick pulses 
  )
