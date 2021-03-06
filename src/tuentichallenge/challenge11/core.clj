(ns tuentichallenge.challenge11.core
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.tools.trace :refer [trace dotrace deftrace trace-forms]]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

(tufte/add-basic-println-handler! {})

(defn indices [pred coll]
   (keep-indexed #(when (pred %2) %1) coll))

(defn flip [f & xs]
   (apply f (reverse xs)))

(defn gauss [a z]
  (/ (* (inc (- z a)) (+ a z)) 2))

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn debug [function & args]
  (if true
  (apply function args)))

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
           {0
            {G__29693 {:color "Red", :target 1},
             G__29696 {:color "Blue", :target 3}},
            3
            {G__29694 {:color "Red", :target 2},
             G__29697 {:color "Blue", :target 4}},
            1 {G__29695 {:color "Green", :target 2}},
            2 {G__29698 {:color "Yellow", :target 0}}},
           :galaxies
           {0 {"Red" 10, "Green" 10},
            1 {"Red" 15, "Green" 15},
            2 {"Blue" 7, "Green" 5},
            3 {"Red" 11},
            4 {"Red" 10, "Green" 10}},
           :colors {"Red" (), "Green" (), "Blue" (), "Yellow" ("Red" "Green")}})

;; Set de colores requeridos para un wormhole
(defn expand-color [problem color]
  (let [subcolors (get-in problem [:colors color])]
    (if (empty? subcolors)
      #{color}
      (set subcolors))))

(defn expand-colors [problem colors]
  (apply clojure.set/union  (map (partial expand-color problem) colors)))

(defn check-color-1 [problem colors color]
  ;; o lo contiene o lo contiene la versión expandida
  (or (contains? colors color) (contains? (expand-colors problem colors) color)))

(defn check-color [problem colors color]
  ;; color es un subcomponente de uno de los colors
  ;; no se está teniendo en cuenta
  (p :check-color
  (let [colors (set colors)
        info (get-in problem [:colors color])]
    (if (> (count info) 0)
      ;; color es un color padre y nosotros tenemos los subcomponentes
      (or (contains? colors color) (clojure.set/subset? (set info) colors))
      ;; color es un color simple y nosotros lo tenemos, o bien es un color simple y uno de nuestros colores lo tiene como subcomponente
      (check-color-1 problem colors color)))))

(defn color-cost [problem galaxy color]
  (get-in problem [:galaxies galaxy color]))

(defn color-costs [problem galaxy colors]
  (map (partial color-cost problem galaxy) colors))

(defn wormhole-is-reachable [problem colors wormhole]
  (println "is wormhole reachable" wormhole (check-color problem colors (:color wormhole)))
  (check-color problem colors (:color wormhole)))

;; wormholes accesibles de forma directa
(defn only-reachable-wormholes [problem wormholes available-colors]
  (p :only-reachable-wormholes
  (into {} (->> wormholes
    (filter (fn [[k v]] (wormhole-is-reachable problem available-colors v)))))))

;; wormholes accesibles de forma transitiva pero no directa
(defn only-unused-cost-wormholes [problem wormholes unused-colors]
  (p :only-unused-cost-wormholes
  (->> wormholes
    (filter (fn [[k wormhole]] (clojure.set/subset? (set (expand-color problem (:color wormhole))) (set (keys unused-colors)))))
    )))

(defn merge-min-costs [a b]
  (if (< (:cost a) (:cost b)) a b))

(defn galaxy-colors [problem galaxy]
  (get-in problem [:galaxies galaxy]))

(def c (atom 0))
(def unprocessed-wormholes (atom {}))

(defn multiple-color-cost [problem solution wormhole galaxy]
  (debug println "-- multiple color cost --")
  (let [colors (expand-color problem (:color wormhole))]
  (reduce (fn [solution color] 
    (let [cost (or (get (:unused-colors solution) color) 0)]
      (debug println "processing color" color "cost" cost)
      (if (> cost 0)
        (-> solution
          (update :cost + cost)
          (assoc :valid-cost true)
          (update :unused-colors dissoc color))
        (-> solution
         (assoc :valid-cost false))
        )))
    (-> solution (assoc :valid-cost false))
    colors)
  ))

(defn single-color-cost [problem solution wormhole galaxy]
  (debug println "-- single color cost --")
  (let [color (:color wormhole)
        cost (or (get (:unused-colors solution) color) 0)]
     (debug println "processing color" color "cost" cost)
    (if (> cost 0)
      (-> solution
          (update :cost + cost)
          (assoc :valid-cost true)
          (update :unused-colors dissoc color))
      (-> solution
         (assoc :valid-cost false)))))

;; Obtener soluciones directas a partir de una galaxia, un estado y un coste máximo
(defn direct-paths* [problem galaxy unused-colors acc-cost]
  (debug println)
  (debug println "direct-paths, galaxy" galaxy "unused-colors" unused-colors "acc-cost" acc-cost)
  ;; Obtener los colores en este punto
  ;; Obtener los wormholes posibles, por cada uno guardar la galaxia, el coste y el state en el listado de soluciones
  ;; Por cada galaxia directa desde las soluciones encontradas, obtener los posibles wormholes y guardar como solución la conexión más pequeña para cada galaxia target
  ;;   (Detener el proceso si se vuelve a un nodo que ya es una solución)
  (let [galaxy-colors (let [a (galaxy-colors problem galaxy)] (debug println "galaxy-colors" a) a)
        available-colors (p :available-colors (keys galaxy-colors))
        wormholes (get @unprocessed-wormholes galaxy)
        ;a (println "the wms" wormholes)
        reachable-wormholes (only-reachable-wormholes problem wormholes available-colors)
        ;a (println "reach" reachable-wormholes)
        unused-cost-wormholes (only-unused-cost-wormholes problem wormholes unused-colors)
        merged-wormholes (merge {} reachable-wormholes unused-cost-wormholes)
        a (debug println "merged" merged-wormholes)
        solutions (p :main-reducer (reduce (fn [acc [wh-key wormhole]]
                                (debug println "reducing" @c wormhole acc)
                                (swap! c inc)
                                ;(println ": " @c galaxy wormhole)
                                (if true
                                  ;; Solución hacia el target del wormhole
                                  (do
                                  (assoc acc (:target wormhole)
                                    (-> 
                                     {;; Colores disponibles para cálculo de coste = colores disponibles + colores de la galaxia (merged with min)
                                      :unused-colors (merge-with min unused-colors galaxy-colors)
                                      ;; Asociar coste base
                                      :cost acc-cost}

                                      ;; Coste acumulado + (coste de wormhole si no se dispone de sus colores)
                                      ;; Por cada color del wormhole, comprobar si ya se tiene, si no se tiene comparar con uno de unused-costs,
                                      ;; y usar el más conveniente actualizando unused-costs
                                      ;; El proceso es el mismo si es :is-unused-cost, la única diferencia es que el código ha podido entender
                                      ;; que el wormhole es alcanzable aunque no sea de forma directa
                                      ((fn [solution]
                                        (debug println "solution" solution)
                                        (let [single-color-cost (single-color-cost problem solution wormhole galaxy)
                                              multiple-color-cost (multiple-color-cost problem solution wormhole galaxy)]
                                          (cond
                                            (and (:valid-cost single-color-cost) (:valid-cost multiple-color-cost))
                                              (if (> (:cost single-color-cost) (:cost multiple-color-cost))
                                                (trace "using multiple color cost" multiple-color-cost)
                                                (trace "using single color cost" single-color-cost))
                                            (:valid-cost single-color-cost)
                                              (trace "usign single color cost" single-color-cost)
                                            :else
                                            (trace "using multiple color cost" multiple-color-cost)))))
                                      ))))) {} merged-wormholes))
        a (swap! unprocessed-wormholes update galaxy (fn [i] (apply dissoc i (keys merged-wormholes))))
        ;d (println "Processed ")
        a (debug println "solutions" solutions)
        ;b (println "Unprocessed:" (get-in problem [:unprocessed-wormholes]))
        ;c (println "The keys and galaxy" (keys merged-wormholes) galaxy)
        ]
    solutions))

(defn direct-paths [problem]
  (reset! c 0)
  (reset! unprocessed-wormholes (:wormholes problem))
  (loop [solutions {} calls {0 {:unused-colors {} :cost 0}}]
    ; Hijos directos de cada galaxia solucionada
    (let [new-calls (reduce
                      (fn [acc [call-galaxy call]]
                        (if (> (count (get @unprocessed-wormholes call-galaxy)) 0)
                          (let [result (direct-paths* problem call-galaxy (:unused-colors call) (:cost call))]
                            (merge-with merge-min-costs acc result))))
                      {} calls)]
      (if (> (count new-calls) 0)
        (recur (merge-with merge-min-costs solutions new-calls) new-calls)
        (merge-with merge-min-costs solutions new-calls))
    )
))

;; build a graph
;; find direct minimal paths
(defn solve-case* [problem]
  (let [direct-paths (direct-paths problem)]
    (debug println "-- DIRECT PATHS --")
    (debug clojure.pprint/pprint direct-paths)
    (map (fn [k]
      (if (get direct-paths k)
        (:cost (get direct-paths k))
        -1)
      )
      (range 1 (count (get problem :galaxies))))
    ))


(defn solve-case [idx case]
  (if-not (= idx 26)
    nil
    (do
    (println "solving case " idx)
    (debug clojure.pprint/pprint case)
    (str "Case #" (inc idx) ": " "0 " (clojure.string/join " " (profile {} (let [a (solve-case* case)] (debug clojure.pprint/pprint a) a )))))))




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
            (update-in acc [(:source wormhole)] assoc (gensym) (dissoc wormhole :source)))
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
