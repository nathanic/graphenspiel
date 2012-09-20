(ns graphenspiel.core
  (:use [graphenspiel.math :only [distance]]
        [graphenspiel.sound :only [play-simple-sound]]
        )
  (:import java.lang.Thread))

; why the crap is this not in clojure.core?
(defn dissoc-in
  "Dissociates an entry from a nested associative structure where ks is a
  sequence of keys and returns a new nested structure."
  [m [k & ks]]
  (if ks
    (assoc m k (dissoc-in (get m k) ks))
    (dissoc m k)))

(def ^:dynamic *sim-interval* 
  "time between state ticks in milliseconds" 
  10)

(def ^:dynamic *pulse-step-size*
  "distance traveled by a pulse in one tick"
  1)

(def ^:dynamic *pulse-generation-interval*
  "how long, in ticks, a node goes between birthing pulses"
  (* 5 50))

(def ^:dynamic *reaction-duration*
  200)

; the current tick index of the simulation
(defonce tick* (atom 0))

(defonce id-seed*
  (atom 0))

(defn fresh-id!
  "get a fresh id (a keyword for referencing nodes) with an optional prefix"
  ([] 
  (fresh-id! "id"))
  ([prefix]
  (keyword (str prefix (swap! id-seed* inc)))))

; (fresh-id!)

(def quit* 
  "when true, the update thread will [eventually] quit"
  (atom false))

(def p 
  "less annoying alias for (partial)"
  partial)

; let's just set up a simple initial state and work with that for now
(def initial-state
  {:graph {:nodes {:src0 {:kind :source
                          :pos [50 50]
                          ; :pulse-interval 100
                          :created 0 ; beginning of time
                          :id :src0
                          }
                   :snk0 {:kind :sink
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
(defonce the-state (atom initial-state))

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
  ; some fun interactive graph jiggery-pokery
  (def st @the-state)
  (edge-nodes st (get-in st [:graph :edges 0]))
  (edges-from-node st :src0)

  (pprint
    (add-node st 
              {:id :snk1
               :pos [100 100]
               :kind :sink}
              [[:src0 :snk1]]))
  (swap! the-state 
         add-node {:id :snk1
                   :pos [100 100]
                   :kind :sink}
                  [[:src0 :snk1]])
  (swap! the-state assoc-in [:graph :nodes :snk1 :pos] [140 130])

  (swap! the-state 
         add-node {:id :snk2
                   :pos [200 100]
                   :kind :sink}
                  [[:src0 :snk2]
                   [:src2 :snk2]
                   ])
  (swap! the-state assoc-in [:graph :nodes :snk2 :pos] [200 70])

  (pprint @the-state)
  (swap! the-state
         add-node {:id :src1
                   :pos [200 200]
                   :kind :source
                   :created @tick* }
                  [[:src1 :snk0]
                   [:src1 :snk1] ])
  (swap! the-state assoc-in [:graph :nodes :src1 :pos] [210 230])
  (swap! the-state assoc-in [:graph :nodes :src1 :pos] [300 300])
  (swap! the-state assoc-in [:graph :nodes :src2 :pos] [300 100])

  (swap! the-state
         add-node {:id :src2
                   :pos [300 200]
                   :kind :source
                   :created @tick* }
                  [[:src2 :snk1] ])
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


(defmethod handle-tick :source
  [st node]
  (if (time-to-add-pulses? node) 
    (add-pulses st (edges-from-node st (:id node)))
    st))


(defmethod handle-tick :sink
  [st node]
  ; clean up any expired reactions
  (if-let [{:keys [start dur]} (:reacting node)] 
    (if (> @tick* (+ start dur))
      (dissoc-in st [:graph :nodes (:id node) :reacting])
      #_(assoc-in st [:graph :nodes (:id node)]
                (dissoc node :reacting))
      st)
    st)) ; this structure feels a bit weird

; something is halting the update thread when we get to an arrival
; we should set up an almost-arrived state, and step the simulation
; to see what happens.

(comment
  (def st'' @the-state)
  (sim-step st'')
  )
(comment
  (def st @the-state)
  (def node (get-in st [:graph :nodes :snk0]))
  (:pulses (add-pulses st (edges-from-node st (:id node))))
  (reset! tick* 100)
  (swap! the-state handle-tick (get-in @the-state [:graph :nodes :src0]))

  (def st' (assoc-in st [:graph :nodes :snk0 :reacting]
                     {:start @tick*, :dur *reaction-duration*}))
  (def node' (get-in st' [:graph :nodes :snk0]))
  (pprint 
    (handle-tick st' node'))
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

(defmulti react-arrival 
  (fn [st pulse] 
    ;(map :kind (edge-nodes st (:edge pulse)))
    (let [[src snk] (edge-nodes st (:edge pulse))]
      [(:kind src) (:kind snk)])))

; we'll eventually have more node types
(defmethod react-arrival [:source :sink]
  [st pulse]
  #_(println "arrival!")
  (let [snk-id (-> pulse :edge second)
        snk    (get-in st [:graph :nodes snk-id])]
    (when-not (get-in st [:config :muted]) 
      (play-simple-sound snk))
    (assoc-in st [:graph :nodes snk-id :reacting]
              {:start @tick*, :dur   10    }))) 

(defn react-arrivals 
  [st] 
  (let [arrivals (filter (p arrived? st) (:pulses st))]
    (reduce (fn [st p] 
              ; (println "reacting to arrival of pulse:" p)
              (react-arrival st p)) 
            st arrivals)))

(comment
  (def st @the-state)
  (reduce (fn [n x] (inc n)) 0 [1 2 3])
  (react-arrival st {:edge [:src0 :snk0]})
  (react-arrivals st)
  )
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


  (def states (atom []))
  (do
    ; step the simulation once
    (swap! tick* inc)
    (swap! the-state sim-step)
    (swap! states conj @the-state)
    )


  (swap! the-state assoc-in [:graph :nodes :reacting] {:start @tick*, :dur 10})
  (swap! quit* not)

  ; i was getting a problem because i accidentally linked a src back to itself
  ; and i didn't have a method for react-arrival [:source :source]
  ; let's surgically remove that
  (swap! the-state update-in [:graph :edges] 
         (p remove (fn [[a b]] (= a b))))

  (def backup-st @the-state)
  (spit "graphenspiel-state-dump.clj" (with-out-str (pprint @the-state)))

  (do
    (require '[graphenspiel.drawing :as drawing])
    (drawing/start)

    (def sim-thread 
      (future (try
                (sim-loop)
                (catch Throwable e
                  (println "Exception in simulation thread:" e)
                  (.printStackTrace e)))))) 

  (do  
    (future-cancel sim-thread)
    (reset! the-state initial-state))


  )



(comment
  (def m (ns-map 'graphenspiel.core))
  (pprint (keys m))
  (ns-publics 'graphenspiel.core)


  [(for [sink (filter #((= (:kind %) :sink)) 
                      (vals (get-in st [:graph :nodes])))] 
     [id (:id sink)])]
  )


; next steps
;   click to create pulses?
;   drag/drop pulses
;   java sound api midi notes?




(comment
  ; let's try some java sound api midi 
  (import '[javax.sound.midi MidiSystem Synthesizer MidiChannel MidiUnavailableException])

  (def synth (MidiSystem/getSynthesizer))
  ; MidiUnavailableException: Requested device not installed!
  ; dang it.

  )


