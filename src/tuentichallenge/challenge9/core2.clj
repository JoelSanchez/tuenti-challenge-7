(ns tuentichallenge.challenge9.core2
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn insert [vec pos item] 
    (apply merge (subvec vec 0 pos) item (subvec vec pos)))

(defn apply-vector [pos v]
  (merge-with + pos v))

(defn double-vector [v]
  (into {} (map (fn [[k i]] [k (* i 2)]) v)))

(defn vector-by-type [type]
  (case type
    :up {:x 0 :y -1}
    :right {:x 1 :y 0}
    :down {:x 0 :y 1}
    :left {:x -1 :y 0}))

(defn turn-vector-type [type orientation]
  (case type
    :up (if (= orientation :cw) :right :left)
    :right (if (= orientation :cw) :down :up)
    :down (if (= orientation :cw) :left :right)
    :left (if (= orientation :cw) :up :down)))

(defn side-to-vector [side]
  (case side
    :up :down
    :right :left
    :down :up
    :left :right))

(defn nxt-short [nxts {:keys [s c d pos v-type way] :as state}]
  (if (zero? s) nxts (conj nxts (-> state (update :s dec) (update :pos apply-vector (vector-by-type v-type)) (assoc :piece :s)))))

(defn nxt-double [nxts {:keys [s c d pos v-type way] :as state}]
  (if (zero? d) nxts (conj nxts (-> state (update :d dec) (update :pos apply-vector (double-vector (vector-by-type v-type))) (assoc :piece :d)))))

(defn nxt-curve [nxts {:keys [s c d pos v-type way] :as state} orientation]
  (if (zero? c) nxts
    (conj nxts (-> state (update :c dec)
                   (assoc :v-type (turn-vector-type v-type orientation))
                   (update :pos apply-vector (vector-by-type v-type))
                   (assoc :piece :c)))))

(defn filter-nxts [nxts way]
  (reduce (fn [acc i] (if (.contains way (:pos i))
                          acc
                          (conj acc i))) [] nxts))

(defn find-first [f coll]
  (first (filter f coll)))

(defn nxt-ways [nxts]
  ;(clojure.pprint/pprint nxts)
  (map (fn [i] (update i :way conj (assoc (:pos i) :piece (:piece i)))) nxts))

(defn impossible-solution [{:keys [s c d pos v-type way] :as state}]
  (let [min-y (Math/abs (:y pos))
        min-x (Math/abs (:x pos))
        max-moves (+ s c (* d 2))
        no-turns (and (not (zero? (:y pos))) (not (zero? (:x pos))) (zero? c))]
    (or no-turns (> (dec (+ min-y min-x)) max-moves))))

(defn solved-solution [{:keys [s c d pos v-type way] :as state}]
  (if (and (= pos {:x 0 :y 1}) (= v-type :up))
      (count (:way state))
      nil))

(defn nxt [{:keys [s c d pos v-type way] :as state}]
  (if (impossible-solution state)
    []
    (if-let [solution (solved-solution state)]
      (do (println "solved" {:solution solution}) (println) (println)
            {:solution solution})
      (let [nxts (-> [] (nxt-short state) (nxt-double state) (nxt-curve state :cw) (nxt-curve state :countercw) (filter-nxts way) (nxt-ways))]
        ;(println "nxt debug") (clojure.pprint/pprint nxts)
        nxts))))

(defn indices [pred coll]
   (keep-indexed #(when (pred %2) %1) coll))

(defn encode-node [n]
  (let [n (clojure.string/split n #"-")]
    {:pos {:x (nth n 2) :y (nth n 3)} :side (nth n 4) :kind (nth 0) :n (nth 1) :s (nth 5) :c (nth 6) :d (nth 7)}))

(defn decode-node [n]
  (clojure.string/join [(:kind n) (:n n) (get-in n [:pos :x]) (get-in n [:pos :y]) (:side n) (:s n) (:c n) (:d n) ]))

(defn get-state-from-way [orig-state way]
  (reduce (fn [acc i] ) {} way))

(defn get-distances [base-state n way distance]
  (let [state (decode-node n)]
    ))

(defn remove-keys [pred m]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra [state start target]
  (loop [q (priority-map start 0) r {}]
    (when-let [[v d] (peek q)]
      (if (= v target)
        d
        (let [dists (remove-keys r (get-distances state v r d))]
          (recur (merge-with min (pop q) dists) (assoc r v d)))))))

(defn solve-case [{:keys [s c d] :as state}]
  (time (dijkstra state 1 "0-1-bottom")))

(defn process-case [idx case]
  (println "case: " idx case)
  (let [[s c d] (read-string (str "[" case "]"))
        solution (solve-case {:all (+ s c d) :s s :c c :d d})]
    (str "Case #" (inc idx) ": " solution)
    ))

(defn challenge-from-file [input output]
    (with-open [rdr (clojure.java.io/reader input :encoding "UTF-8")]
        (doall (spit output (clojure.string/join "\n" (map-indexed process-case (drop 1 (line-seq rdr))))))))