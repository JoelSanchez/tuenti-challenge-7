(ns tuentichallenge.challenge6.core
  (:require [clojure.math.combinatorics :as combo]))


(defn dijkstra [g src]
  (loop [dsts (assoc (zipmap (keys g) (repeat nil)) src 0)
         curr src
         unvi (apply hash-set (keys g))]
    (if (empty? unvi)
      dsts
      (let [unvi  (disj unvi curr)
            nextn (first (sort-by #(% dsts) unvi))
            nrds  (zipmap (keys g) (map #(select-keys % unvi) (vals g)))]
        (if (empty? (curr nrds))
          (recur dsts nextn unvi)
          (let [cdst  (curr dsts)
                roads (select-keys (curr g) unvi)
                reslt (zipmap (keys dsts)
                        (map #(if-let [rd (% roads)]
                                (let [idst (% dsts)
                                      sum  (+ cdst (% roads))]
                                  (if (or (nil? idst)
                                          (< sum idst))
                                    sum idst))
                                (% dsts)) (keys dsts)))]
            (recur reslt nextn unvi)))))))

(defn get-floor [floor max-floor]
  (reduce (fn [acc i] (if (> i floor) (assoc acc (keyword (str i)) (- i floor)) acc)) {} (range 1 (inc max-floor))))

(defn base-graph [max-floor]
  ;;(println "base-graph" max-floor)
  (into {} (map (fn [i] [(keyword (str i)) (get-floor i max-floor)]) (range 1  max-floor))))

(defn process-shortcuts [shortcuts]
  (reduce (fn [acc i] (assoc acc (keyword (str (get i 0))) {(keyword (str (get i 1))) (get i 2)})) {} (map #(read-string (str "[" % "]")) shortcuts )))

(defn graph [shortcuts max-floor]
  (let [graph (base-graph max-floor)]
    (merge-with merge graph (process-shortcuts shortcuts))))

(defn solve-case [{:keys [shortcuts max-floor] :as full-case}]
  ;;(println "solve-case" full-case)
  ;;(println "graph" (graph shortcuts max-floor))
   (dijkstra (graph shortcuts max-floor) :1))

(defn process-case [idx case]
  (str "Case #" (inc idx) ": " (solve-case case)))

(defn process-lines [lines]
  ;; (println "process-lines")
  (loop [cases [] idx 1]
    (do ;; (println "process-lines-1 cases" cases "idx" idx)
    ;;(println (get lines idx))
    (if-let [raw-line (get lines idx)]
      (let [line (read-string (str "[" raw-line "]"))
            base-case {:max-floor (first line) :shortcuts (last line)}
            shortcuts (map #(get lines (+ 1 idx %)) (range (:shortcuts base-case)))
            case (assoc base-case :shortcuts shortcuts)]
        ;;(println "process-lines-1-1-1 line " line "base-case" base-case "shortcuts" shortcuts "case" case "range" (range (+ 1 idx (:shortcuts base-case))))
        (recur (conj cases case) (+ idx (count shortcuts) 1)))
      cases))))

(defn challenge-from-file [input output]
	(with-open [rdr (clojure.java.io/reader input)]
		(doall (spit output (clojure.string/join "\n" (map-indexed process-case (process-lines (vec (line-seq rdr)))))))))