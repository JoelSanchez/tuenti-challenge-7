(ns tuentichallenge.challenge1.core)

(defn solve-case [[people maxes]]
  (Math/ceil (/ (reduce (fn [acc i] (+ acc i)) 0 (read-string (str "[" maxes "]"))) 8)))

(defn process-file [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (map solve-case (partition 2 (drop 1 (line-seq rdr)))))))

(defn output-solution [idx solution]
  (println (str "Case #" (inc idx) ": " (int solution))))

(defn challenge [filename]
  (map-indexed output-solution (process-file filename)))