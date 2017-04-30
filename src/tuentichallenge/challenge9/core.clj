(ns tuentichallenge.challenge9.core
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

(defn solved-solution-2 [{:keys [s c d pos v-type way] :as state}]
  (let [new-state (-> state
                      (update :pos apply-vector (vector-by-type v-type)))]
    (if (= (:pos new-state) {:x 0 :y 0})
      (count (:way new-state))
      nil)))

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

(defn solve-case* [{:keys [s c d pos v-type way all] :as state}]
  (println "solve-case | state-n" state)
  (let [n (nxt state)]
     ;(println "nxt") (clojure.pprint/pprint n)
    (if (or (get n :solution) (empty? n))
      (if (empty? n) [] [(do (get n :solution))])
      (doall (reduce (fn [acc i]
                        ; (println "last-sol" (get (last acc) :solution) (last acc))
                        (if (or (get (last acc) :solution) (= (last acc) all) (> (count acc) 50))
                          acc
                          (doall (concat acc (solve-case* i))))) [] n)))))

(defn solve-case [{:keys [s c d] :as state}]
  (if (< c 2)
    0
    (let [state (-> state (assoc :v-type :right)
                          (assoc :pos {:x 0 :y 0})
                          (assoc :way [{:x 0 :y 0}])
                          (update :c dec))]
      (let [solutions (doall (solve-case* state))]
        (if (> (count solutions) 0) (do (println "the solutions" solutions) (apply max solutions)) 0)))))

(defn process-case [idx case]
  (println "case: " idx case)
  (let [[s c d] (read-string (str "[" case "]"))
        solution (solve-case {:all (+ s c d) :s s :c c :d d})]
    (str "Case #" (inc idx) ": " solution)
    ))

(defn challenge-from-file [input output]
    (with-open [rdr (clojure.java.io/reader input :encoding "UTF-8")]
        (doall (spit output (clojure.string/join "\n" (map-indexed process-case (drop 1 (line-seq rdr))))))))