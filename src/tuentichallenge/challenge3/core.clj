(ns tuentichallenge.challenge3.core)

(defn nearest-power-of-two [x]
  (Math/ceil (/ (Math/log x) (Math/log 2))))

(defn process-case [idx case]
	(str "Case #" (inc idx) ": " (int (nearest-power-of-two (read-string case)))))

(defn challenge-from-file [filename]
	(with-open [rdr (clojure.java.io/reader filename)]
		(doall (map-indexed process-case (drop 1 (line-seq rdr))))))