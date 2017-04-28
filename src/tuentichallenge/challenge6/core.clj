(ns tuentichallenge.challenge6.core
  (:require [clojure.data.priority-map :refer [priority-map]]))


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

(defn map-vals [f m]
  ;(println "map-vals m" m "f" f)
  (into {} (for [[k v] m] [k (f v)])))
 
(defn remove-keys [pred m]
  ;(println "remove-keys | m" m "pred" pred)
  (select-keys m (filter (complement pred) (keys m))))

(defn gauss [a z]
  (/ (* (inc (- z a)) (+ a z)) 2))

(defn dijkstra-2
  "Computes single-source shortest path distance in a directed graph.
   Given a node n, (f n) should return a map with the successors of n 
   as keys and their (non-negative) distance from n as vals. Returns 
   distance to target or nil if no path."
  [start target f]
  (loop [q (priority-map start 0) r {}]
    (println "r" r)
    (when-let [[v d] (peek q)]
      ;(println v d target)
      (if (= v target)
        d
        (let [dists (->> (f v) (remove-keys r) (map-vals (partial + d)))]
          ;(println "dists" dists "f v" (f v) "remove-keys" (remove-keys r (f v)))
          (recur (merge-with min (pop q) dists) (assoc r v d)))))))

(defn graph-adapter [graph]
  (fn [n]
    ;; (println "calling " n)
    (get graph n)))

(defn get-floor [floor max-floor]
  (reduce (fn [acc i]
            (if (> i floor)
              (assoc acc (keyword (str i)) (gauss floor (dec i)))
              (assoc acc (keyword (str i)) 0)))
          {} (range 1 (inc max-floor))))

(defn base-graph [max-floor]
  ;;(println "base-graph" max-floor)
  (into {} (map (fn [i] [(keyword (str i)) (get-floor i max-floor)]) (range 1  max-floor))))

(defn process-shortcuts [shortcuts]
  (reduce (fn [acc i]
            (let [new-val (get i 2)
                  src (keyword (str (get i 0)))
                  tgt (keyword (str (get i 1)))
                  old-val (get-in acc [src tgt])]
              ; (println "comparing" old-val new-val "src" src "tgt" tgt)
              (if (or (and old-val (< new-val old-val)) (not old-val))
                (assoc-in acc [src tgt] new-val)
                acc)))
          {} (map #(read-string (str "[" % "]")) shortcuts )))

(defn graph [shortcuts max-floor]
  (let [graph (base-graph max-floor)]
    (merge-with merge graph (process-shortcuts shortcuts))))

(defn solve-case [{:keys [shortcuts max-floor] :as full-case}]
  (println)(println)
  (clojure.pprint/pprint full-case)
  (let [g (graph shortcuts max-floor)]
    (clojure.pprint/pprint g)
    (dijkstra-2 :1 (keyword (str max-floor)) (graph-adapter g))))

(defn process-case [idx case]
  (println "CASE " (inc idx))
  (if (= (inc idx) 45)
    (System/exit 0))
  (let [c (solve-case case)]
    ;(println "solution" c)
    (str "Case #" (inc idx) ": " c)))

(defn process-lines [lines]
  ; (println "the lines: " lines)
  (loop [cases [] idx 1]
    (do ;; (println "process-lines-1 cases" cases "idx" idx)
    ;;(println (get lines idx))
    (if-let [raw-line (do (if (> idx (dec (count lines))) nil (nth lines idx)))]
      (let [line (read-string (str "[" raw-line "]"))
            base-case {:max-floor (first line) :shortcuts (last line)}
            shortcuts (map #(nth lines (+ 1 idx %)) (range (:shortcuts base-case)))
            case (assoc base-case :shortcuts (vec shortcuts))]
        ;;(println "process-lines-1-1-1 line " line "base-case" base-case "shortcuts" shortcuts "case" case "range" (range (+ 1 idx (:shortcuts base-case))))
        (recur (conj cases case) (+ idx (count shortcuts) 1)))
      cases))))

(defn challenge-from-file [input output]
	(with-open [rdr (clojure.java.io/reader input)]
    (doall
      (spit output (clojure.string/join "\n" (map-indexed process-case (process-lines (vec (line-seq rdr)))))))))
