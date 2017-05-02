(ns tuentichallenge.challenge11.core
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.tools.trace :refer [trace dotrace deftrace trace-forms]]))

(defn indices [pred coll]
   (keep-indexed #(when (pred %2) %1) coll))

(defn flip [f & xs]
   (apply f (reverse xs)))

(defn gauss [a z]
  (/ (* (inc (- z a)) (+ a z)) 2))

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-keys [pred m]
  (select-keys m (filter (complement pred) (keys m))))

(defn way-colors [case way]
  (let [a (reduce (fn [acc node]
            (let [colors (get-in case [:galaxies node])]
              (clojure.set/union acc (set (keys colors)))))
          #{} way)]
    (if (seq a)
      a
      #{})))

(defn way-color-costs [case way]
  (into {} (map (fn [color] [color 0]) (way-colors case way))))

(def a '{:wormholes
         {0 ({:color "Blue", :target 3} {:color "Red", :target 1}),
          3 ({:color "Blue", :target 4} {:color "Red", :target 2}),
          1 ({:color "Green", :target 2}),
          2 ({:color "Yellow", :target 0})},
         :galaxies
         {0 {"Red" 10, "Green" 10},
          1 {"Red" 15, "Green" 15},
          2 {"Blue" 7, "Green" 5},
          3 {"Red" 11},
          4 {"Red" 10, "Green" 10}},
         :colors {"Red" (), "Green" (), "Blue" (), "Yellow" ("Red" "Green")}})

(defn check-color [problem colors color]
  (let [colors (set colors)
        info (get-in problem [:colors color])]
    (if (> (count info) 0)
      (or (contains? colors color) (clojure.set/subset? colors (set info)))
      (contains? colors color))))

(defn color-cost [problem galaxy color]
  (get-in problem [:galaxies galaxy color]))

(defn color-costs [problem galaxy colors]
  (map (partial color-cost problem galaxy) colors))

(defn wormhole-colors [problem wormhole]
  (let [subcolors (get-in problem [:colors (:color wormhole)])]
    (if (empty? subcolors)
      #{(:color wormhole)}
      (set subcolors))))

(defn wormhole-cost [problem galaxy wormhole]
  (trace galaxy wormhole)
  (trace (color-costs problem galaxy (wormhole-colors problem wormhole)))
  (trace (wormhole-colors problem wormhole))
  (reduce + (or (color-costs problem galaxy (wormhole-colors problem wormhole)) [0]) ))

(defn wormhole-is-reachable [problem colors wormhole]
  (trace (check-color problem colors (:color wormhole))))

(defn wormhole-is-visited [problem solutions wormhole]
  (if solutions
    (.contains (keys solutions) (:target wormhole))
    false
  ))

;; wormholes accesibles de forma directa
(defn only-reachable-wormholes [problem available-colors wormholes]
  (->> wormholes
    (filter (partial wormhole-is-reachable problem available-colors))))

;; wormholes no accesibles de forma directa
(defn only-unreachable-wormholes [problem solutions available-colors wormholes]
  (->> wormholes
    (filter (comp not (partial wormhole-is-reachable problem available-colors)))
    ))

;; wormholes accesibles de forma transitiva pero no directa
(defn only-unused-cost-wormholes [problem unreachable-wormholes unused-colors]
  (->> unreachable-wormholes
    (filter (fn [i] (clojure.set/subset? (set (wormhole-colors problem i)) (set (keys unused-colors)))))
    ))


(defn merge-min-costs [a b]
  (if (< (:cost a) (:cost b)) a b))


(def current-unreachable-wormholes (atom []))

;; Obtener soluciones directas a partir de una galaxia, un estado y un coste máximo
(defn direct-paths* [problem solutions galaxy colors unused-colors acc-cost]
  (println)
  (println "direct-paths, galaxy" galaxy "colors" colors "solutions" solutions "unused-colors" unused-colors "acc-cost" acc-cost)
  ;; Obtener los colores en este punto
  ;; Obtener los wormholes posibles, por cada uno guardar la galaxia, el coste y el state en el listado de soluciones
  ;; Por cada galaxia directa desde las soluciones encontradas, obtener los posibles wormholes y guardar como solución la conexión más pequeña para cada galaxia target
  ;;   (Detener el proceso si se vuelve a un nodo que ya es una solución)
  (let [galaxy-colors (let [a (get-in problem [:galaxies galaxy])] (println "galaxy-colors" a) a)
        available-colors (clojure.set/union colors (keys galaxy-colors))
        wormholes (get-in problem [:wormholes galaxy])
        reachable-wormholes (trace "reachable wormholes" (only-reachable-wormholes problem available-colors wormholes))
        unreachable-wormholes (reset! current-unreachable-wormholes (trace "unreachable wormholes" (concat @current-unreachable-wormholes (only-unreachable-wormholes problem solutions available-colors wormholes))))
        unused-cost-wormholes (trace "unused cost wormholes" (only-unused-cost-wormholes problem unreachable-wormholes unused-colors))
        new-solutions (reduce (fn [acc wormhole]
                                (if (get solutions (:target wormhole))
                                  acc
                                  ;; Solución hacia el target del wormhole
                                  (assoc acc (:target wormhole)
                                    (-> 
                                     {;; Colores acumulados, ya que el color del wormhole se cancela
                                      :colors colors
                                      ;; Están sin usar los colores sin usar anteriores + los de la galaxia - los colores del wormhole
                                      :unused-colors (merge unused-colors (apply dissoc galaxy-colors (wormhole-colors problem wormhole)))}


                                      ;; Coste acumulado + (coste de wormhole si es necesario usar un color, o bien unused-color)
                                      ((fn [solution]
                                        (let [wh-cost (or (wormhole-cost problem galaxy wormhole) 0)
                                              unused-cost (or (get unused-colors (:color wormhole)) 0)]
                                          ;; Hay un coste de wormhole?
                                          (if (and (clojure.set/subset? (wormhole-colors problem wormhole) colors))
                                            (assoc solution :cost acc-cost)
                                            ;; Se debería usar unused-costs?
                                            (if (and (> unused-cost 0) (> wh-cost unused-cost))
                                              (-> solution (assoc :cost (+ acc-cost unused-cost)) (assoc-in [:unused-colors (:color wormhole)] wh-cost))
                                              (-> solution (assoc :cost (+ acc-cost wh-cost)))
                                              )
                                            )
                                          )))
                                      )))) {} (concat reachable-wormholes (map (fn [i] (assoc i :is-unused-cost true)) unused-cost-wormholes)))

        a (do (println "new-solutions" new-solutions))
        all-solutions (reduce (fn [acc [galaxy solution]]
                        (merge-with merge-min-costs acc (direct-paths* problem acc galaxy (:colors solution) (:unused-colors solution) (:cost solution)))) (merge solutions new-solutions) new-solutions)]
    (println "merged solutions" (merge solutions all-solutions))
    (merge solutions all-solutions)))

(defn direct-paths [problem]
  (direct-paths* problem {} 0 #{} {} 0))

(defn reverse-paths* [problem direct-paths acc unreachable-wormhole]
  ()
  )

(defn reverse-paths [problem direct-paths]
  (reduce (fn [acc i] (reverse-paths* problem direct-paths acc i)) {} direct-paths))

;; build a graph
;; find direct minimal paths
(defn solve-case* [problem]
  (let [direct-paths (direct-paths problem)
        reverse-paths (reverse-paths problem direct-paths)
    ]
    (println "-- DIRECT PATHS --")
    (clojure.pprint/pprint direct-paths)
    (println " -- UNREACHABLE --")
    (clojure.pprint/pprint @current-unreachable-wormholes)
    (println "-- REVERSE PATHS --")
    (clojure.pprint/pprint reverse-paths)))


(defn solve-case [idx case]
  (println "solving case " idx)
  (clojure.pprint/pprint case)
  (println "Case #" idx ": " (solve-case* case)))




(defn subvec-length [v start length]
  (subvec v start (+ start length)))

(defn apply-processor [lines processor times]
  (loop [lines lines acc-values [] acc-used-lines 0 times times]
    (if (zero? times)
      {:used-lines acc-used-lines :value acc-values :remaining lines}
      (let [{:keys [value used-lines]} (processor lines)]
        (recur (drop used-lines lines) (conj acc-values value) (+ acc-used-lines used-lines) (dec times))))))

(defn index-wormholes [wormholes]
  (reduce (fn [acc wormhole]
            (update acc (:source wormhole) conj (dissoc wormhole :source)))
          {} wormholes))

(defn index-galaxies [galaxies]
  (reduce (fn [acc wormhole]
            (assoc acc (:name wormhole) (:energies wormhole)))
          {} galaxies))

(defn index-energies [energies]
  (reduce (fn [acc energy]
            (assoc acc (:color energy) (:seconds energy)))
          {} energies))

(defn index-colors [colors]
  (reduce (fn [acc color]
            (assoc acc (:name color) (:subcolors color)))
          {} colors))

(defn process-wormhole [lines]
  (let [l (clojure.string/split (first lines) #"\s")]
    {:used-lines 1
     :value {:color (get l 0) :source (read-string (get l 1)) :target (read-string (get l 2))}}))

(defn process-color [lines]
  (let [l (clojure.string/split (first lines) #"\s")]
    {:used-lines 1
     :value {:name (get l 0) :subcolors (drop 2 l)}}))

(defn process-energy [lines]
  (let [l (clojure.string/split (first lines) #"\s")]
    {:used-lines 1
     :value {:color (get l 0) :seconds (read-string (get l 1))}}))

(def galaxy-idx (atom 0))

(defn process-galaxy [lines]
  (swap! galaxy-idx inc)
  (let [{:keys [used-lines value]} (apply-processor (drop 1 lines) process-energy (read-string (first lines)))]
    {:used-lines (inc used-lines)
     :value {:name (dec @galaxy-idx) :energies (index-energies value)}}))

(defn process-count [lines]
  {:used-lines 1
   :value (read-string (first lines))})

(defn process-case [lines]
  (reset! galaxy-idx 0)
  (let [used-lines 0

        result (apply-processor lines process-count 1)
        lines (:remaining result)
        used-lines (+ used-lines (:used-lines result))
        n-colors (first (:value result))
        

        result (apply-processor lines process-color n-colors)
        lines (:remaining result)
        used-lines (+ used-lines (:used-lines result))
        colors (:value result)

        result (apply-processor lines process-count 1)
        lines (:remaining result)
        used-lines (+ used-lines (:used-lines result))
        n-galaxies (first (:value result))

        result (apply-processor lines process-galaxy n-galaxies)
        lines (:remaining result)
        used-lines (+ used-lines (:used-lines result))
        galaxies (:value result)

        result (apply-processor lines process-count 1)
        lines (:remaining result)
        used-lines (+ used-lines (:used-lines result))
        n-wormholes (first (:value result))

        result (apply-processor lines process-wormhole n-wormholes)
        lines (:remaining result)
        used-lines (+ used-lines (:used-lines result))
        wormholes (:value result)]
    {:remaining lines
     :used-lines used-lines
     :value {:wormholes (index-wormholes wormholes) :galaxies (index-galaxies galaxies) :colors (index-colors colors)}}))


(defn process-file [lines]
  (let [{:keys [used-lines value]} (apply-processor (drop 1 lines) process-case (read-string (first lines)))]
    {:used-lines used-lines
     :value value}))

(defn challenge-from-file [input output]
  (with-open [rdr (clojure.java.io/reader input)]
    (doall
      (let [cases (:value (process-file (vec (line-seq rdr))))]
        (spit output (clojure.string/join "\n" (map-indexed solve-case cases)))
        ))))


(defn debug-parse [input output]
  (with-open [rdr (clojure.java.io/reader input)]
    (doall
      (:value (process-file (vec (line-seq rdr)))))))