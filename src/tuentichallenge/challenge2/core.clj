(ns tuentichallenge.challenge2.core)

(defn apply-points-to-frames [frames min-idx points]
  (into [] (map-indexed (fn [idx i] (if (>= idx min-idx) (+ i points) i)) frames)))

(defn apply-bonus-points [{:keys [rolls frames] :as acc} type jumps]
  (let [roll (get rolls (- (count rolls) (inc jumps)))
        target-idx (:frame-idx roll)
        target (get frames target-idx)
        last-roll (last rolls)]
    (if (and target (= (:type roll) type))
      (update acc :frames #(apply-points-to-frames % target-idx (:score last-roll)))
      acc)))

(defn add-frame [{:keys [rolls frames] :as acc} base-score]
  (if (= (count frames) 10)
    acc
    (let [last-frame-score (if-let [last-frame (last frames)] last-frame 0)
          score (+ last-frame-score base-score)]
      (update acc :frames conj score))))

(defn add-roll [{:keys [rolls frames] :as acc} type score]
  (-> acc
      (update :rolls conj {:type type :score score :frame-idx (count frames)})
      (apply-bonus-points :spare 1)
      (apply-bonus-points :strike 1)
      (apply-bonus-points :strike 2)))

(defn roll-score [{:keys [frames rolls] :as acc} score]
  (let [last-roll (last rolls)]
    (cond
      (and (= (:type last-roll) :frame-start) (= 10 (+ score (:score last-roll))))
        (-> acc
            (add-roll :spare score)
            (add-frame 10))
      (= score 10)
        (-> acc
          (add-roll :strike score)
          (add-frame score))
      (= (:type last-roll) :frame-start)
        (-> acc
          (add-roll :frame-end score)
          (add-frame (+ score (:score last-roll))))
      :else (add-roll acc :frame-start score))))

(defn solve-case [[number rolls]]
  (reduce roll-score {:frames [] :rolls []} (read-string (str "[" rolls "]"))))

(defn process-file [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (map solve-case (partition 2 (drop 1 (line-seq rdr)))))))

(defn output-solution [idx solution]
  (println (str "Case #" (inc idx) ": " (clojure.string/join " " (:frames solution)))))

(defn challenge [filename]
  (map-indexed output-solution (process-file filename)))