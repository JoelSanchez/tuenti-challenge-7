(ns tuentichallenge.challenge4.core
  (:require [clojure.math.combinatorics :as combo]))

(defn missing-length-hyp [a b]
  (Math/sqrt (+ (* a a) (* b b))))

(defn missing-length [hyp a]
  (Math/round (Math/sqrt (- (* hyp hyp) (* a a)))))

(defn solve-case [lengths]
  (let [combs (combo/combinations (rest lengths) 3)
        valid-combs (filter (fn [i]
                              (let [i (sort i)]
                                ;(println "the i" i)
                                ;(println (< (nth i 2) (+ (nth i 1) (nth i 0))))
                                (< (nth i 2) (+ (nth i 1) (nth i 0))))) combs)]
    ;; (println "valid" valid-combs)
    (if-let [result (first (sort (map #(reduce + %) valid-combs)))]
      result
      "IMPOSSIBLE")))

(defn process-case [idx case]
	(str "Case #" (inc idx) ": " (solve-case (read-string (str "[" case "]")))))

(defn challenge-from-file [input output]
	(with-open [rdr (clojure.java.io/reader input)]
		(doall (spit output (clojure.string/join "\n" (map-indexed process-case (drop 1 (line-seq rdr))))))))