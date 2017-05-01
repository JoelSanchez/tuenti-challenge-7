(ns tuentichallenge.challenge10.core2
  (:require [clj-time.core :as t]
            [clojure.java.io :as io]
            [clj-time.format :as f])
  (:import [java.io File]))

;;
;; Dates
;;
(defn make-date [s]
  (f/parse (f/formatters :year-month-day) s))

(defn next-date [date]
  (t/plus date (t/days 1)))

(defn prev-date [date]
  (t/minus date (t/days 1)))

(defn print-date [d]
  (f/unparse (f/formatter "yyyy-MM-dd") d))


;;
;; Clojure implementation of script.php
;;
(defn crc32 [s]
  (let [CRC (new java.util.zip.CRC32)]
    (. CRC update (. s getBytes))
    (. CRC getValue)))

(defn md5 [s]
  (let [algorithm (java.security.MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn php-password [counter secret1 secret2]
  (loop [idx 0 acc "" counter (mod (* counter secret1) secret2)]
      (if (= idx 10)
        acc
        (recur (inc idx)
               (str acc (char (+ (mod counter 94) 33)))
               (mod (* counter secret1) secret2)))))

(defn modpow [n exp m]
  (-> (biginteger n) (.modPow (biginteger exp) (biginteger m))))

(defn php-counter [seed secret1 secret2]
  (mod (* seed (modpow secret1 10000000 secret2)) secret2))

(defn php-implementation
  ([secret1 secret2 user] (php-implementation secret1 secret2 user nil))
  ([secret1 secret2 user pwd-hash]
    (let [counter-seed (if pwd-hash (crc32 pwd-hash) (crc32 user))
          counter (php-counter counter-seed secret1 secret2)
          password (php-password counter secret1 secret2)]
      {:password password :hash (md5 password)})))



(defn find-files*
  "Find files in `path` by `pred`."
  [path pred]
  (filter pred (-> path io/file file-seq)))

(defn find-files
  "Find files matching given `pattern`."
  [path pattern]
  (find-files* path #(re-matches pattern (.getName ^File %))))

(defn secret-from-line [s]
  (-> s (clojure.string/replace #"\$secret[0-9] = " "") (clojure.string/replace #";" "") (read-string)))

(defn get-secrets []
  (let [files (find-files "resources/challenge10/versions" (re-pattern (str ".*?" "script\\.php")))]
    (into {} (doall (map (fn [file]
                            (with-open [rdr (clojure.java.io/reader file)]
                              (let [lines (line-seq rdr)
                                    secrets (map secret-from-line [(nth lines 6) (nth lines 7)])
                                    date (clojure.string/replace (.getName file) #"\.script\.php" "")]
                                [date secrets]))) files)))))

(def secrets (get-secrets))


(defn solve-case-date [date user {:keys [hash]}]
  (let [[secret1 secret2] (get secrets (print-date date))]
    (php-implementation secret1 secret2 user hash)))

(defn solve-case [{:keys [user dates] :as case}]
  (reduce (fn [acc date] (solve-case-date date user acc)) nil dates))

(defn process-case [idx case]
  (let [result (solve-case case)]
    (println "processing case " (inc idx) result)
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
