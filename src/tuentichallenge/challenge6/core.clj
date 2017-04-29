(ns tuentichallenge.challenge6.core
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn get-distances [case floor way distance]
  (-> (if (> floor 1) {(dec floor) distance} {})
      ((fn [m] (if-not (= floor (:max-floor case)) (assoc m (inc floor) (+ distance floor)))))
      ((fn [m] (reduce  (fn [acc [idx v]]
                          (if-let [s-val (get m idx)]
                            (assoc acc idx (min s-val (+ v distance)))
                            (assoc acc idx (+ v distance))))
                        m (get (:shortcuts case) floor))))))

(defn remove-keys [pred m]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra [case start target]
  (loop [q (priority-map start 0) r {}]
    ; (println "r" r)
    (when-let [[v d] (peek q)]
      (if (= v target)
        d
        (let [dists (remove-keys r (get-distances case v r d))]
          ; (println "dists" dists) (println)
          (recur (merge-with min (pop q) dists) (assoc r v d)))))))

(defn process-shortcuts [shortcuts]
  (reduce (fn [acc i]
            (let [new-val (get i 2)
                  src (get i 0)
                  tgt (get i 1)
                  old-val (get-in acc [src tgt])]
              (if (or (and old-val (< new-val old-val)) (not old-val))
                (assoc-in acc [src tgt] new-val)
                acc)))
          {} shortcuts))

(defn solve-case [{:keys [shortcuts max-floor] :as full-case}]
  (time (dijkstra full-case 1 max-floor)))

(def case-idx (atom 0))

(defn process-case [case]
  (swap! case-idx inc)
  (println "solving case " @case-idx ", " (count (:shortcuts case)) "shortcuts, " (:max-floor case) "floors")
  (str "Case #" @case-idx ": " (solve-case case)))

(defn process-lines [lines]
  (loop [cases [] idx 1]
    (if-let [raw-line (do (if (> idx (dec (count lines))) nil (nth lines idx)))]
      (let [line (read-string (str "[" raw-line "]"))
            base-case {:max-floor (first line) :shortcuts (last line)}
            raw-shortcuts (map (fn [i] (read-string (str "[" (nth lines (+ 1 idx i)) "]"))) (range (:shortcuts base-case)))
            case (assoc base-case :shortcuts (process-shortcuts raw-shortcuts))]
        (recur (conj cases case) (+ idx (count raw-shortcuts) 1)))
      cases)))

(defn challenge-from-file [input output]
	(with-open [rdr (clojure.java.io/reader input)]
    (doall
      (spit output (clojure.string/join "\n" (pmap process-case (process-lines (vec (line-seq rdr)))))))))
