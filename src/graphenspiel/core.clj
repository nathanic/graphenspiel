(ns graphenspiel.core
  (:use [graphenspiel.math :only [distance]])
  (:import java.lang.Thread))

(def ^:dynamic *sim-interval* 
  "time between state ticks in milliseconds" 
  50)

(def ^:dynamic *pulse-step-size*
  "distance traveled by a pulse in one tick"
  1)

(def ^:dynamic *pulse-generation-interval*
  "how long, in ticks, a node goes between birthing pulses"
  100)

; the current tick index of the simulation
(defonce tick* (atom 0))

(def quit* 
  "when true, the update thread will [eventually] quit"
  (atom false))

(def p 
  "less annoying alias for (partial)"
  partial)

; let's just set up a simple initial state and work with that for now
(def initial-state
  {:graph {:nodes {:src0 {:kind ::source
                          :pos [50 50]
                          ; :pulse-interval 100
                          :created 0 ; beginning of time
                          :id :src0
                          }
                   :snk0 {:kind ::sink
                          :pos [100 200]
                          :id :snk0
                          }
                   }
           :edges [[:src0 :snk0]] ; list of pairs of node keys
           } 
   :pulses [{:edge [:src0 :snk0]
             :pos  50
             }]
   })

; i'm a little bothered by the duplication of the node :id in the above
; but it simplifies passing data around if nodes know their own id

; the once piece of shared mutable state
(def the-state (atom initial-state))

(defn edge-nodes 
  "returns the pair of node records referenced by an edge"
  [state edge]
  [(get-in state [:graph :nodes (first edge)])
   (get-in state [:graph :nodes (second edge)])])

(defn edges-from-node
  "returns the list of all edges directed out from a given node,
  which is specified by id"
  [st node-id]
  (filter (fn [[src snk]] (= node-id src)) 
          (get-in st [:graph :edges])))

(defn add-node 
  [st node edges]
  (-> st
    (assoc-in [:graph :nodes (:id node)] node)
    (update-in [:graph :edges] concat edges)))

(comment
  (def st @the-state)
  (edge-nodes st (get-in st [:graph :edges 0]))
  (edges-from-node st :src0)
  (add-node st {:id :snk1
                :created @tick*
                :pos [100 100]
                :kind ::sink
                
                })
  )

(defn arrived? 
  "a pulse has arrived when its :pos is greater than the length of its :edge"
  [st pulse]
  (>= (:pos pulse) 
      (apply distance (map :pos (edge-nodes st (:edge pulse))))))


(defn time-to-add-pulses?
  [node]
  (= 0
     (mod (- @tick* (:created node)) 
          *pulse-generation-interval*)))

(defn add-pulses 
  [st edges]
  (let [pulses (for [edge edges] 
                 {:pos 0, :edge edge})] 
    (update-in st [:pulses] concat pulses)))

(defmulti handle-tick (fn [st node] (:kind node)))


(defmethod handle-tick ::source
  [st node]
  (if (time-to-add-pulses? node) 
    (add-pulses st (edges-from-node st (:id node)))
    st))

(comment
  (def st @the-state)
  (def node (get-in st [:graph :nodes :src0]))
  (:pulses (add-pulses st (edges-from-node st (:id node))))
  (reset! tick* 100)
  (swap! the-state handle-tick (get-in @the-state [:graph :nodes :src0]))
  )

(defmethod handle-tick :default
  [st node]
  #_(println "WARNING: handle-tick :default called")
  st)


(comment
  (handle-tick st (get-in st [:graph :nodes :src0]))
  )


; what if one node handler alters the graph, removing another node?
; we might call a tick handler for a node that doesn't exist anymore.
; so if we allow node handlers to update the graph we should start
; doing an existence check before calling handle-tick,
; which also means it won't be a slick (reduce) call anymore.
(defn tick-nodes
  "transform the state by evaluating all of the node tick handlers"
  [st]
  (reduce handle-tick st (vals (get-in st [:graph :nodes]))))

; this guy's job is *not* handling arrival reactions
(defn advance-pulses
  "transform the state by advancing the position of all the in-transit pulses" 
  [st]
  (assoc st :pulses
         (mapv (fn [{:keys [pos] :as pulse}] 
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
; there still has to be some way to use the fact that the
; drawing thread is never going to mutate the-state...
; TODO: grok the locking semantics of STM refs
(defn sim-loop
  []
  (while (not @quit*)
    (swap! tick* inc)
    (swap! the-state sim-step)
    (Thread/sleep *sim-interval*)
    )) 


; these are for swank interaction, !!REMOVE WHEN NOT NEEDED!!
(use '[clojure repl pprint data])

(comment
  (reset! the-state initial-state)
  (def st @the-state)
  (def st' (sim-step st))
  (first (diff st st') )  ; st-only
  (second (diff st st') ) ; st'-only

  (with-out-str (time (sim-step st)))

  (def st'
    (loop [x 100, st st] 
      (if (pos? x) 
        (recur (dec x) (sim-step st))
        st)))

  (do
    ; step the simulation once
    (swap! tick* inc)
    (swap! the-state sim-step))

  (def sim-thread
    (future
      (sim-loop)))
  (future-cancel sim-thread)

  (swap! quit* not)

  (require '[graphenspiel.drawing :as drawing])
  (drawing/start)
  )



(comment
  (def m (ns-map 'graphenspiel.core))
  (pprint (keys m))
  (ns-publics 'graphenspiel.core)
  )


; next steps
;   pulse regeneration
;   have the live gui up while ticking state
;   arrival reactions
