; goal: make a little noise when a node receives a pulse
(ns graphenspiel.sound
  (:use overtone.live ))

(definst sin-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (sin-osc freq)
     vol))

(definst ding
  [note 60 velocity 100]
  (let [freq (midicps note)
        snd  (sin-osc freq)
        env  (env-gen (perc 0.1 0.8) :action FREE)]
    (* velocity env snd)))

(definst poly-ding
  [note 60 velocity 100 gate 1]
  (let [freq (midicps note)
        amp  (/ velocity 127.0)
        snd  (sin-osc freq)
        env  (env-gen (adsr 0.001 0.1 0.6 0.3) gate :action FREE)]
    (* amp env snd)))

(definst harpsichord [freq 440]
  (let [duration 1]
    (*
      (line:kr 1 1 duration FREE)
      (pluck (* (white-noise) (env-gen (perc 0.001 5) :action FREE))
             1 1 (/ 1 freq) (* duration 2) 0.25))))


; turrible: extract the numeric part of :snkN and pick a midi note from it
(defn hacky-extract-number
  [node-id]  
  (Integer. (.substring (str :snk0) 4))) 

(defn play-simple-sound
  [node]
  ; (ding)
  ; (poly-ding)
  (let [freq ; (rand-nth (map midi->hz (range 40 70)))
             (midi->hz (+ 60 (* 5 (hacky-extract-number (:id node)))))
        ] 
    (harpsichord freq))
  )

(comment
  (sin-wave)
  (play-simple-sound)

  (demo (sin-osc (+ 1000 (* 600 (lf-noise0 12))) 0.3))

  )
