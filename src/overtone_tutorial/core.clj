(ns overtone-tutorial.core)

(use 'overtone.live)
(require '[overtone.inst.synth :as synths])
(require '[overtone.inst.drum :as drums])

(definst saw-wave [freq 440 attack 0.85 sustain 0.1 release 4 vol 0.4] 
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(defn note->hz [music-note]
  (midi->hz (note music-note)))

(defn saw2 [music-note]
  (saw-wave (note->hz music-note)))

(defn play-chord [a-chord]
  (doseq [note a-chord] (saw2 note)))

(defn progression [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :C4 :major)))
  (at (m (+ 4 beat-num)) (play-chord (chord :G3 :major)))
  (at (m (+ 8 beat-num)) (play-chord (chord :F3 :sus4)))
  (at (m (+ 12 beat-num)) (play-chord (chord :F3 :major)))
  (at (m (+ 16 beat-num)) (play-chord (chord :G3 :major)))
  (apply-at (m (+ 16 beat-num)) progression m (+ 16 beat-num) [])
)

(defn play [time notes sep sample]
  (let [note (first notes)]
    (when note
      (at time (sample)) 
      (let [next-time (+ time sep)]
        (apply-at next-time play [next-time (rest notes) sep sample])))))

; setup a sound for our metronome to use
(def kick (sample (freesound-path 2086)))

; setup a tempo for our metronome to use
(def one-twenty-bpm (metronome 120))

; this function will play our sound at whatever tempo we've set our metronome to 
(defn looper [nome sound]    
    (let [beat (nome)]
        (at (nome beat) (sound))
        (apply-by (nome (inc beat)) looper nome sound [])))

(defn beat [m beat-num]
  (map 
    (fn [n] 
      (at (m (+ 4 beat-num n)) 
          (drums/kick :amp 3))) 
    (iterate + 1))  
)

(defonce metro (metronome 120))
(defn beat2 [m beat-num sequence sample]
  (let [beat (first sequence) nextBeat (second sequence)]
    (when beat
      (at (m (+ beat-num beat)) (sample))
      (when nextBeat 
        (let [next-beat-num (+ beat-num nextBeat)]
         (apply-at (m next-beat-num) beat2 m beat-num (rest sequence) sample []))))))

(defn note-seq [sample sequence] 
  (beat2 metro (metro) sequence sample))

(defn reload [] (use 'overtone-tutorial.core :reload))