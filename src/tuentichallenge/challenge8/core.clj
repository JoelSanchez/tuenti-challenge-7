(ns tuentichallenge.challenge8.core)

(defmacro case+
  "Same as case, but evaluates dispatch values, needed for referring to
   class and def'ed constants as well as java.util.Enum instances."
  [value & clauses]
  (let [clauses (partition 2 2 nil clauses)
        default (when (-> clauses last count (== 1))
                  (last clauses))
        clauses (if default (drop-last clauses) clauses)
        eval-dispatch (fn [d]
                        (if (list? d)
                          (map eval d)
                          (eval d)))]
    `(case ~value
       ~@(concat (->> clauses
                   (map #(-> % first eval-dispatch (list (second %))))
                   (mapcat identity))
           default))))

(defn get-category [c]
  (case+ (byte (Character/getType c))
    Character/COMBINING_SPACING_MARK "Mc"
    Character/CONNECTOR_PUNCTUATION "Pc"
    Character/CONTROL "Cc"
    Character/CURRENCY_SYMBOL "Sc"
    Character/DASH_PUNCTUATION "Pd"
    Character/DECIMAL_DIGIT_NUMBER "Nd"
    Character/ENCLOSING_MARK "Me"
    Character/END_PUNCTUATION "Pe"
    Character/FINAL_QUOTE_PUNCTUATION "Pf"
    Character/FORMAT "Cf"
    Character/INITIAL_QUOTE_PUNCTUATION "Pi"
    Character/LETTER_NUMBER "Nl"
    Character/LINE_SEPARATOR "Zl"
    Character/LOWERCASE_LETTER "Ll"
    Character/MATH_SYMBOL "Sm"
    Character/MODIFIER_LETTER "Lm"
    Character/MODIFIER_SYMBOL "Sk"
    Character/NON_SPACING_MARK "Mn"
    Character/OTHER_LETTER "Lo"
    Character/OTHER_NUMBER "No"
    Character/OTHER_PUNCTUATION "Po"
    Character/OTHER_SYMBOL "So"
    Character/PARAGRAPH_SEPARATOR "Zp"
    Character/PRIVATE_USE "Co"
    Character/SPACE_SEPARATOR "Zs"
    Character/START_PUNCTUATION "Ps"
    Character/SURROGATE "Cs"
    Character/TITLECASE_LETTER "Lt"
    Character/UNASSIGNED "Cn"
    Character/UPPERCASE_LETTER "Lu"))

(defn char->common-number [n]
  (if-let [unicode-name (Character/getName (Character/codePointAt (str n) 0))]
    (do
      (cond
        (.contains unicode-name "ZERO") 0
        (.contains unicode-name "ONE") 1
        (.contains unicode-name "TWO") 2
        (.contains unicode-name "THREE") 3
        (.contains unicode-name "FOUR") 4
        (.contains unicode-name "FIVE") 5
        (.contains unicode-name "SIX") 6
        (.contains unicode-name "SEVEN") 7
        (.contains unicode-name "EIGHT") 8
        (.contains unicode-name "NINE") 9
        :else nil
        ))
    nil))

(defn string->common-numbers [s]
  (clojure.string/join (map (fn [i] (if (= (get-category i) "Nd") (char->common-number i) i)) (seq s))))

(def BEGIN "^")
(def IGNORE_WHITESPACE "\\p{Zs}*")
(def CAPTURE_SIGN "(\\-?)")
(def END "$")
(def CAPTURE_ANYTHING_EXCEPT_WHITESPACE "([^\\p{Zs}]+)")

(defn process-match [match]
  ;; ignore empty captures and full match
  (let [match (filter not-empty (drop 1 match))]
    (cond
       (and (= 2 (count match)) (= "-" (first match))) (str "-" (second match))
       (= 1 (count match)) (first match)
       :else nil)))

(defn ensure-number-chars [s]
  (loop [idx 0]
    (if (> (inc idx) (count s))
      s
      (if (or (= (get s idx) \-) (= (get-category (get s idx)) "Nd"))
        (recur (inc idx))
        nil))))

(defn match-number [n]
  (let [matches (re-seq (re-pattern (str BEGIN IGNORE_WHITESPACE CAPTURE_SIGN IGNORE_WHITESPACE CAPTURE_ANYTHING_EXCEPT_WHITESPACE IGNORE_WHITESPACE END)) n)]
    ;; several valid numbers: illegal
    (if-not (= (count matches) 1)
      nil
      (if-let [match (process-match (first matches))]
        (ensure-number-chars match)
        nil))))

(defn dec->hex [d]
  (-> (new BigInteger d 10) (.toString 16)))

(defn solve-case [s]
  (if-let [unicode-n (match-number s)]
    (dec->hex (string->common-numbers unicode-n))
    "N/A"))

(defn process-case [idx case]
  (println "case: " idx case)
  (let [solution (solve-case case)]
    (println "solution: " solution)
    (println)
    (str "Case #" (inc idx) ": " solution)))

(defn challenge-from-file [input output]
    (with-open [rdr (clojure.java.io/reader input :encoding "UTF-8")]
        (doall (spit output (clojure.string/join "\n" (map-indexed process-case (drop 1 (line-seq rdr))))))))