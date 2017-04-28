(ns tuentichallenge.challenge5.core
  (:require [clojure.core.async :refer [<!! >!! >! <! close! go go-loop]]
            [com.gearswithingears.async-sockets :refer :all]))
 
(defn choose-smart [strategy options value]
  (case strategy
    :lose (if (.contains options "+hit") (if (> 15 value) "+stand" "+hit") "+bet2")
    :win (if (.contains options "+hit")  (if (> 15 value) "+hit" "+stand") "+bet10")))

(defn choose [strategy options value]
  (rand-nth options))

(defn echo-everything [strategy socket]
  (go-loop [options [] state nil value 0]
    (when-let [line (<! (:in socket))]
      (println line "options" options "state" state)
      (cond
        (= line "## OPTIONS START")
          (recur [] :options-started value)
        (= line "## OPTIONS END")
          (recur options nil value)
        (clojure.string/starts-with? line "## CURRENT VALUES")
          (recur options state (get (read-string (clojure.string/replace line "## CURRENT VALUES " "")) 0))
        (= line "## CHOOSE")
          (let [option (rand-nth options)]
            (println "-- sending --" option)
            (>! (:out socket) (choose strategy options value))
            (recur options state value))
        (= state :options-started)
          (recur (conj options line) state value)
        :else (recur options state value)))))

(defn tuenti-loser []
  (echo-everything :lose (socket-client 2121 "52.49.91.111")))

(defn tuenti-winner []
  (echo-everything :win (socket-client 2121 "52.49.91.111")))