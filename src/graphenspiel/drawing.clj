(ns graphenspiel.drawing
  (:use graphenspiel.core
        graphenspiel.math
        quil.core
        quil.applet))

; swanky li'l helpers
(use '[clojure repl pprint])

; drawing config
(def cx 400) ; window dimensions
(def cy 400)
(def node-radius 50)
(def pulse-radius 10)

; contains the id of the node the user is currently dragging, or nil.
(def dragging-node* (atom nil))

(defn mouse-pos [] [(mouse-x) (mouse-y)])

(defn- draw-edges
  [state]
  (stroke-weight 5)
  (stroke 128 128 128)
  (doseq [edge (get-in state [:graph :edges])]
    (->> (edge-nodes state edge)
      (map :pos)
      (apply line))))

(defn draw-node*
  "helper to draw a node of a given color.  if not specified, 
  the stroke color defaults to black."
  [node color stroke-color]
  (apply fill color)
  (apply stroke (or stroke-color [0 0 0])) 
  (let [[x y] (:pos node)]
    (ellipse x y node-radius node-radius)))

(defmulti draw-node (fn [state node] (:kind node)))

(defmethod draw-node :source
  [st node]
  (draw-node* node [255 0 0]
              (if (= (:linking-from st) (:id node))
                [255 255 255] 
                nil)))

(defmethod draw-node :sink
  [st node]
  (let [color (if (contains? node :reacting)
                [200 200 40]
                [128 128 255]) ]
    (draw-node* node color
                (if (= (:linking-from st) (:id node))
                  [255 255 255] 
                  nil))))

(defn- draw-nodes
  [st]
  (stroke 0 0 0)
  (doseq [node (vals (get-in st [:graph :nodes]))]
    (draw-node st node)))

(defn- draw-pulses
  [state]
  (no-stroke)
  (fill 0 200 30)
  (doseq [pulse (get-in state [:pulses])
          :let [[org dst] (edge-nodes state (:edge pulse))
                [x y]     (linterp (:pos org) (:pos dst) (:pos pulse)) ]]
    (ellipse x y pulse-radius pulse-radius)))


(defn- draw []
  (let [state @the-state]
    (background 180)
    (draw-edges state)
    (draw-nodes state)
    (draw-pulses state)
    ))

(defn- setup []
  (shape-mode :center)
  (frame-rate 24)
  (smooth)
  (stroke 0)
  (stroke-weight 5)
  (fill 255 25)

  ; TODO: reference the state atom in this sketch instance's state?
  ; that way we could have multiple independent instances within one process
  )

(defn- mouse-clicked
  []
  ; if ctrl is held down and we hit a node, 
  ; then we enter alter-connection mode
  ; set up some state to note the ID of the clicked node
  (let [st @the-state] 
    (if-let [clicked-nid (first (hovered-node-ids st (mouse-pos)))]
      ; the user clicked directly on a node
      (if-let [from-nid (:linking-from @the-state)] 
        ; we are choosing a destination node
        ; TODO: see if this link already exists and if so destroy it instead
        ; TODO: enforce src->sink digraph
        (swap! the-state (fn [st] 
                           (-> st 
                             ; N.B. that this does not necessarily respect the src->sink
                             ; edge order expected elsewhere in the sim
                             (update-in [:graph :edges] conj [from-nid clicked-nid])
                             (dissoc :linking-from))))

        ; not choosing a dest, but maybe a source
        (when (= (key-as-keyword) :control)
          (swap! the-state assoc :linking-from clicked-nid)))

      ; the user didn't click on a node  
      (do
        ; clear any pending link alteration
        (swap! the-state dissoc :linking-from)
        (case (mouse-button)
          ; create a new sink node at the point of the mouse click
          :left (let [id (fresh-id! "snk")]
                  (swap! the-state
                         add-node
                         {:id id
                          :pos [(mouse-x) (mouse-y)]
                          :kind :sink}
                         [[:src0 id]]))
          ; create a new source node
          :right (let [id (fresh-id! "src")]
                   (swap! the-state
                          (fn [st]
                            (add-node
                              st
                              {:id id
                               :pos [(mouse-x) (mouse-y)]
                               :kind :source
                               :created @tick*}
                              ; for now just hook it up to all sinks
                              (for [sink (filter #(= (:kind %) :sink)
                                                 (vals (get-in st [:graph :nodes])))]
                                [id (:id sink)]))))))))) 
  ; the foregoing is the longest clojure function i've ever written
  ; need to break that monster up


  (comment
    (def id :foo)
    (def st @the-state)
    (def nds (get-in st [:graph :nodes]))

    )
  ; TODO: create edges to only nearby sources
  ; what's nearby?  some kind of radius setting
  ; naively we could calc the distance to all sources
  ; could do a first pass just fencing on x/y,
  ; and only do radius compare within a certain range
  )

(defn key-pressed []
  (case (raw-key) 
    \r (reset! the-state initial-state)

    \q (do    
         (reset! quit* true)
         (applet-exit (current-applet)))

    nil))

(defn over-node?
  [node mpos]
  (<= (distance (:pos node) mpos) 
      node-radius))

(defn hovered-node-ids
  "node ids of any nodes below mpos"
  [st mpos]
  (->> (get-in st [:graph :nodes])
    (map (fn [[id node]]
           (if (over-node? node mpos)
             id
             nil)))
    (remove nil?)))

(comment
  (def st initial-state)
  (over-node? {:pos [1 1]} [1 1])
  (over-node? {:pos [1 1]} [100 100])
  (over-node? {:pos [1 1]} [25 25])
  (map (fn [[k v]] (str k ": " v)) {:foo 42, :bar "baz", :yup :nope})
  (hovered-node-ids st [100 100])

  (def mpos [100 100])
  (remove nil? (map (fn [[id node]]
                      (if (over-node? node mpos)
                        id
                        nil))
                    (get-in st [:graph :nodes])))
  )

(defn mouse-dragged
  []
  (when-let [nid @dragging-node*]
    (swap! the-state assoc-in [:graph :nodes nid :pos] (mouse-pos))))

(defn mouse-pressed
  []
  ; see if we have a drag target
  (when-let [nid (first (hovered-node-ids @the-state (mouse-pos)))]

    
    
    ; i would like to pop the clicked node to the front of the z-order,
    ; but right now my nodes are not actually stored in a deterministic order...
    (reset! dragging-node* nid)))

(defn mouse-released
  []
  ; cancel drag
  (reset! dragging-node* nil))


; TODO: applet stuff like gtrak's elastic collision demo?
; swing widgets in the owning frame for configuring behavior
(defn start
  []
  (defsketch minimal-nodes
             :title "Graphenspiel!"
             :setup setup
             :draw draw
             :size [cx cy]
             :keep-on-top true

             :key-pressed key-pressed
             :mouse-clicked mouse-clicked
             :mouse-dragged mouse-dragged
             :mouse-pressed mouse-pressed
             :mouse-released mouse-released))


(comment
  (start)
  )


