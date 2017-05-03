(ns tuentichallenge.challenge9.core
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn evenize [n]
  (if (odd? n) (dec n) n))

(defn solve-case* [{:keys [s c d] :as state}]
  (-> state

    ;; setters
    (assoc :solution 0)
    (assoc :line-length 0)
    (assoc :used-ss 0)
    (assoc :used-ds 0)
    (assoc :used-cs 0)

    ((fn [i] (println i) i))

    ;; double reducer
    ((fn [i]
        (let [ds (evenize (:d i))]
          (-> i
            (update :solution + ds)
            (update :line-length + (* ds 2))
            (update :d - ds)
            (update :used-ds + ds)))))

    ((fn [i] (println "double reducer" i) i))

    ;; single reducer
    ((fn [i]
        (let [ss (- (evenize (:s i)) 2)]
          (if (> ss 0)
            (-> i
              (update :solution + ss)
              (update :line-length + (/ ss 2))
              (update :s - ss)
              (update :used-ss + ss))
            i)
          )))

    ((fn [i] (println "single reducer" i) i))

    ;; curve reducer
    ((fn [i]
        (if (>= (:c i) 8)
          (let [cs (* 4 (Math/floor (/ (evenize (- (:c i) 4)) 4)))]
            (if (> cs 0)
              (-> i
                (update :solution + cs)
                (update :c - cs)
                (update :used-cs + cs))
              i)
            )
          i)))

    ((fn [i] (println "curve reducer" i) i))

    ;; base connector
    ((fn [i]
      (-> i
        (update :solution + 2)
        (update :c - 2)
        (update :used-cs + 2))))

    ((fn [i] (println "base connector" i) i))

    ;; pair connector
    ((fn [i] 
        (-> i
            (update :c - 2)
            (update :solution + 2)
            (update :used-cs + 2))))

    ((fn [i] (println "pair connector" i) i))

    ;; remaining singles and doubles
    ((fn [i] 
        (if (> (evenize (:s i)) 0)
          (let [ds (:d i)
                ss (evenize (:s i))]
            (-> i (update :d - ds)
                  (update :s - ss)
                  (update :used-ss + ss)
                  (update :used-ds + ds)
                  (update :line-length + (/ (+ (* ds 2) ss) 2))
                  (update :solution + (+ ds ss))))
          i)))

    ((fn [i] (println "remaining singles and doubles" i) i))

    ;; s upstairs
    ((fn [i] 
        (if (and (>= (:used-ss i) 2) (>= (:c i) 2))
          (-> i
              (update :c - 2)
              (update :solution + 2)
              (update :used-cs + 2)
              (assoc :used-s-upstairs true))
          i)))

    ((fn [i] (println "s upstairs" i) i))

    ;; d upstairs
    ((fn [i] 
        (if (and (= (mod (int (:used-cs i)) 4) 0)
                 (>= (int (:used-cs i)) 8)
                 (>= (:d i) 1)
                 (> (:line-length i) 0)
                 (not (:used-s-upstairs i)))
          (-> i
              (update :d - 1)
              (update :solution + 1)
              (update :used-ds + 1))
          i)))

    ((fn [i] (println "d upstairs" i) i))

    

    ;; curves over single or double
    ((fn [i] 
        (if (and (= (:used-ss i) 0)
                 (= (:used-ds i) 0)
                 (or (= (:d i) 1) (= (:s i) 1))
                 (not (and (= (:d i) 1) (= (:s i) 1)))
                 (> (:used-cs i) 0)
                 (= (mod (int (:used-cs i)) 4) 0))
          (-> i
              (assoc :c 0)
              (assoc :d 0)
              (assoc :s 0)
              (update :used-ds + (:d i))
              (update :used-cs + (:c i))
              (update :solution + (:s i) (:d i) (:c i)))
          i)))

    ((fn [i] (println "curves over single or double" i) i))))


(defn solve-case [{:keys [s c d] :as state}]
  (cond
    (< c 4) 0
    (and (= d 0) (= s 0)) 4
    :else (:solution (solve-case* state))))

(defn process-case [idx case]
  (println "case: " idx case)
  (let [[s c d] (read-string (str "[" case "]"))
        solution (solve-case {:all (+ s c d) :s s :c c :d d})]
    (str "Case #" (inc idx) ": " (int solution))
    ))

(defn challenge-from-file [input output]
    (with-open [rdr (clojure.java.io/reader input :encoding "UTF-8")]
        (doall (spit output (clojure.string/join "\n" (map-indexed process-case (drop 1 (line-seq rdr))))))))