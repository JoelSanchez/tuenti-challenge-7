(ns tuentichallenge.challenge10.core2
  (:require [clojure.java.shell :refer [sh]]
            [clj-time.core :as t]
            [clj-time.format :as f]))

(defn make-date [s]
  (f/parse (f/formatters :year-month-day) s))

(defn next-date [date]
  (t/plus date (t/days 1)))

(defn prev-date [date]
  (t/minus date (t/days 1)))

(defn print-date [d]
  (f/unparse (f/formatter "yyyy-MM-dd") d))

(defn call-script
  ([date user pwd-hash]
    (println "call-script user " user "date" date "pwd-hash" pwd-hash)
    (try
      (apply sh (str "resources/challenge10/versions/" date ".script.php") (filter not-empty [user pwd-hash]))
      (catch Exception e
        (println (.getMessage e))
        )))
  ([date user]
    (call-script date user nil)))

(defn solve-case-date [date user {:keys [hash]}]
  (if-let [result (call-script (print-date date) user hash)]
    (let [explode (clojure.string/split (:out result) #"\s")]
      {:password (get explode 0) :hash (get explode 1)})))

(defn solve-case [{:keys [user dates] :as case}]
  (reduce (fn [acc date] (solve-case-date date user acc)) nil dates))

(defn process-case [idx case]
  (println "processing case " idx)
  (let [result (solve-case case)]
    (str "Case #" (inc idx) ": " (:password result))))

(defn process-dates [dates]
  (apply concat (map (fn [i]
                  (let [v (clojure.string/split i #"\s")
                        times (read-string (get v 1))
                        date (make-date (get v 0))]
                    (repeat times date))) dates)))

(defn process-lines [lines]
  (loop [idx 0 acc []]
    (if (> idx (dec (count lines)))
      acc
      (let [raw-line (clojure.string/split (get lines idx) #"\s")
            user (get raw-line 0)
            n-dates (read-string (get raw-line 1))
            last-date-index (+ (int n-dates) (inc idx))
            dates (subvec lines (inc idx) last-date-index)]
        (recur last-date-index 
               (conj acc {:user user :dates (process-dates dates)}))))))

(defn challenge-from-file [input output]
  (with-open [rdr (clojure.java.io/reader input)]
    (doall
      (let [lines (process-lines (vec (drop 1 (line-seq rdr))))]
        (spit output (clojure.string/join "\n" (map-indexed process-case lines)))
        ))))
