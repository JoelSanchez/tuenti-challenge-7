(ns tuentichallenge.challenge4.core)

(defn sum-indexes [lengths & args]
  (apply + (map #(get lengths %) args)))

(defn get-triangle-sum [lengths start-idx]
  (if (< (+ start-idx 2) (count lengths))
    (sum-indexes lengths start-idx (+ start-idx 1) (+ start-idx 2))))

(defn solve-for-target [lengths target-idx]
  (let [target (get lengths target-idx)]
    (loop [idx (+ target-idx 2)]
      (let [upper (get lengths idx)
            lower (get lengths (dec idx))]
        (if (< (- upper lower) target)
          (+ target upper lower)
          (if (< (inc idx) (count lengths))
            (recur (inc idx))
            nil))))))

(defn solve-case* [lengths idx]
  (if-let [solution (solve-for-target lengths idx)]
    (let [nxt (get-triangle-sum lengths (inc idx))]
      (if (and nxt (< nxt solution))
        (let [nxt-solution (solve-case* lengths (inc idx))]
          (if (< nxt-solution solution)
            nxt-solution
            solution))
        solution))
    (if (< (+ idx 3) (count lengths)) (recur lengths (inc idx)) "IMPOSSIBLE")))

(defn solve-case [lengths]
  (let [lengths (vec (sort (rest lengths)))]
    (solve-case* lengths 0)))

(defn process-case [idx case]
  (str "Case #" (inc idx) ": " (solve-case (read-string (str "[" case "]")))))

(defn challenge-from-file [input output]
	(with-open [rdr (clojure.java.io/reader input)]
		(doall (spit output (clojure.string/join "\n" (map-indexed process-case (drop 1 (line-seq rdr))))))))