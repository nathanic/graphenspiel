(ns nodebeat.core
  (:use [nodebeat.math :only [distance]])
  (:import java.lang.Thread))

(def p 
  "less annoying alias for (partial)"
  partial)

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


(defn arrived? 
  "a pulse has arrived when its :pos is greater than the length of its :edge"
  [st pulse]
  (>= (:pos pulse) 
      (apply distance (map :pos (edge-nodes st (:edge pulse))))))


(comment
  (let [st @the-state] 
    (edge-nodes st (get-in st [:graph :edges 0])))
  )

(def ^:dynamic *sim-interval* 
  "time between state ticks in milliseconds" 
  100)

(def ^:dynamic *pulse-step-size*
  "distance traveled by a pulse in one tick"
  10)

(def quit* (atom false))

; what if one node handler alters the graph, removing another node?
; we might call a tick handler for a node that doesn't exist anymore.
; so if we allow node handlers to update the graph we should start
; doing an existence check before calling handle-tick,
; which also means it won't be a slick (reduce) call anymore.
(defn tick-nodes
  "transform the state by evaluating all of the node tick handlers"
  [st]
  ; not implemented yet
  #_(reduce handle-tick st (get-in st [:graph :nodes]))
  st)

; this guy's job is *not* handling arrival reactions
(defn advance-pulses
  "transform the state by advancing the position of all the in-transit pulses" 
  [st]
  (assoc st :pulses
         (map (fn [{:keys [pos] :as pulse}] 
                    (assoc pulse :pos (+ pos *pulse-step-size*)))
              (:pulses st)))) 

(defn remove-arrived-pulses
  [st]
  (update-in st [:pulses]
             (p remove (p arrived? st))))

(defn react-arrivals 
  [st] 
  ; not implemented yet!
  #_(let [arrivals (filter (p arrived? st) (:pulses st))]
      (doseq [pulse arrivals] 
        (react-arrival pulse)))
  st)

(comment
  ; i should turn these things into unit tests...
  (def st initial-state)
  (:pulses st)
  (:pulses (advance-pulses st))
  
  (def very-arrived-pulse {:pos 1000, :edge [:src0 :snk0]}) 
  (def very-not-arrived-pulse {:pos 0, :edge [:src0 :snk0]}) 
  (arrived? st very-arrived-pulse)
  (arrived? st very-not-arrived-pulse)

  (:pulses
    (remove-arrived-pulses
      (assoc st :pulses [very-arrived-pulse very-not-arrived-pulse])))
  )

(defn sim-step
  [st]
  (-> st
    tick-nodes
    advance-pulses
    react-arrivals
    remove-arrived-pulses))

; when we do swap!, i think the-state is locked for
; the entire next-state computation, which would stall
; drawing.  if we can limit state changes to this thread,
; then we could optimistically (let [st @the-state])
; and just (reset!) when we're done calculating.
; although... user input would probably alter the state
; asynchronously, so i guess we had better lock it.
(defn sim-loop
  []
  (while (not @quit*)
    (swap! the-state sim-step)
    ; (Thread/sleep *sim-interval*)
    )) 


; these are for swank interaction, !!REMOVE WHEN NOT NEEDED!!
(use '[clojure repl pprint data])

(comment

  (def st' (sim-step st))
  (let [[bef aft bth] (diff st st')]
    (def before bef)
    (def after aft)
    (def both bth))

  (prn before)
  (prn after)

  (def st'
    (loop [x 100, st st] 
      (if (pos? x) 
        (recur (dec x) (sim-step st))
        st)))

  (future
    (sim-loop))
  (swap! quit* not)

  )
