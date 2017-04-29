(ns tuentichallenge.challenge6.core
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn indices [pred coll]
   (keep-indexed #(when (pred %2) %1) coll))

(defn gauss [a z]
  (/ (* (inc (- z a)) (+ a z)) 2))

(defn get-distances [case floor way distance]
  (-> {}
      ;; link to next floor worth processing (next shortcut source or final floor)
      ((fn [m]
        (if-not (= floor (:max-floor case))
          (let [next-shortcut-idx (first (indices #(< floor %) (:shortcut-keys case)))
                next-shortcut-floor (get (:shortcut-keys case) next-shortcut-idx)]
            (if next-shortcut-floor
              (assoc m next-shortcut-floor (+ distance (gauss floor (- next-shortcut-floor 1))))
              (assoc m (:max-floor case) (+ distance (gauss floor (- (:max-floor case) 1))))))
          m)))
      ;; link to prev floor worth processing (prev shortcut source or first floor)
      ((fn [m]
        (if-not (<= floor 1)
          (let [prev-shortcut-idx (first (indices #(> floor %) (:shortcut-keys-r case)))
                prev-shortcut-floor (get (:shortcut-keys-r case) prev-shortcut-idx)]
            (if prev-shortcut-floor
              (assoc m prev-shortcut-floor distance)
              (assoc m 1 distance)))
          m)))
      ;; apply shortcuts
      ((fn [m] (reduce  (fn [acc [idx v]]
                          (if-let [s-val (get m idx)]
                            (assoc acc idx (min s-val (+ v distance)))
                            (assoc acc idx (+ v distance))))
                        m (get (:shortcuts case) floor))))))

(defn remove-keys [pred m]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra [case start target]
  (loop [q (priority-map start 0) r {}]
    (when-let [[v d] (peek q)]
      (if (= v target)
        d
        (let [dists (remove-keys r (get-distances case v r d))]
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
            shortcuts (process-shortcuts raw-shortcuts)
            shortcut-keys (vec (sort (keys shortcuts)))
            case (-> base-case (assoc :shortcuts shortcuts) (assoc :shortcut-keys shortcut-keys) (assoc :shortcut-keys-r (vec (reverse shortcut-keys))))]
        (recur (conj cases case) (+ idx (count raw-shortcuts) 1)))
      cases)))

(defn challenge-from-file [input output]
  (reset! case-idx 0)
	(with-open [rdr (clojure.java.io/reader input)]
    (doall
      (spit output (clojure.string/join "\n" (map process-case (process-lines (vec (line-seq rdr)))))))))
