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

(def the-state (atom initial-state))

(defn get-edge-nodes 
  [state edge]
  [(get-in state [:graph :nodes (first edge)])
   (get-in state [:graph :nodes (second edge)])])

