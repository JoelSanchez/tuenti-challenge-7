(ns tuentichallenge.challenge10.core
  (:require [clojure.java.shell :refer [sh]]
            [clj-time.core :as t]
            [clj-time.format :as f]))

(defn crc32 [s]
  (let [CRC (new java.util.zip.CRC32)]
    (. CRC update (. s getBytes))
    (. CRC getValue)))

(defn md5 [s]
  (let [algorithm (java.security.MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn php-counter [seed secret1 secret2]
  (println "php-counter" seed)
  (time
  (loop [idx 0 acc seed]
    (if (= idx 10000000)
      acc
      (recur (inc idx) (mod (* acc secret1) secret2))))))

(defn php-password [counter secret1 secret2]
  (loop [idx 0 acc "" counter (mod (* counter secret1) secret2)]
      (if (= idx 10)
        acc
        (recur (inc idx)
               (str acc (char (+ (mod counter 94) 33)))
               (mod (* counter secret1) secret2)))))

(defn php-implementation
  ([user] (php-implementation user nil))
  ([user pwd-hash]
    (println "user" user "pwd-hash" pwd-hash)
    (let [secret1 6533205
          secret2 2340262
          counter-seed (if pwd-hash (crc32 pwd-hash) (crc32 user))
          counter (php-counter counter-seed secret1 secret2)
          password (php-password counter secret1 secret2)]
      ; (println "using counter" counter)
      [password (md5 password)])))

(defn call-script
  ([user pwd-hash]
    (println "call-script " user "pwd-hash" pwd-hash)
    (apply sh "resources/challenge10/script.php" (filter not-empty [user pwd-hash])))
  ([user]
    (call-script user nil)))

(def a {:user "aaaaaa",
  :dates
  ["2017-01-31"]})

(def b {:user "xoajoj",
  :dates
  ["2013-05-19" "2016-08-20"]})

(def raw-dates ["2013-05-19 2" "2016-08-20 1"])

(comment (defn solve-case [{:keys [user dates] :as case}]
  (loop [dates dates last-hash [] last-date nil]
    (let [next-list-date (first dates)
          next-natural-date (t/plus (f/parse (f/formatters :year-month-day) last-date) (t/days 1))]
      (println "last-hash" last-hash "last-date" last-date "next-list-date" next-list-date "next-natural-date" next-natural-date)
      (recur (rest dates) (php-implementation user last-hash)))

      )))

(defn solve-case [{:keys [user dates] :as case}])

(defn next-date [date]
  (t/plus date (t/days 1)))

(defn make-date [s]
  (f/parse (f/formatters :year-month-day) s))

(defn fill-missing-dates [dates]
  (loop [remaining (rest dates) result [(first dates)]]
    (let [nxt (next-date (first result))]
      (recur remaining (conj nxt)))))


(defn process-case [idx case]
  (str "Case #" idx ": " (solve-case case)))

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
        (println "idx" idx "last-date-index" last-date-index)
        (recur last-date-index 
               (conj acc {:user user :dates (process-dates dates)}))))))

(defn challenge-from-file [input output]
  (with-open [rdr (clojure.java.io/reader input)]
    (doall
      (let [lines (process-lines (vec (drop 1 (line-seq rdr))))]
        (println "Lines")
        (clojure.pprint/pprint lines)
        (println)
        ;(spit output (clojure.string/join "\n" (map-indexed process-case lines)))
        ))))
