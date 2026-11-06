;; longest-palindromic-subsequence: LPS length by interval DP
;;
;; Interval DP: matching ends extend the inner count by two,
;; else the best of dropping either end. Emits the LPS length.
;; Reads stdin tokens, writes the answer to stdout.

(def digit-values
  "Digit character to numeric value."
  (zipmap "0123456789" (range)))

(def separators
  "Whitespace characters that separate input tokens."
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize
  "Split s on whitespace into a vector of token strings."
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

(def min-long
  "Most negative long value: 1 shifted into the sign bit."
  (bit-shift-left 1 63))

(def max-long
  "Largest long value: every bit below the sign bit set."
  (bit-not min-long))

(defn parse-token
  "Parse tok as a decimal long integer with optional sign. Returns
   nil when tok is malformed or out of long range: the sign and ASCII
   digits are matched first, then parse-long reads the value."
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))

(defn read-longs
  "Read all of stdin as a vector of longs."
  []
  (mapv parse-token (tokenize (slurp *in*))))


(let [toks (tokenize (slurp *in*))]
  (if (empty? toks)
    (println 0)
    (let [s (nth toks 0) n (count s)]
      (if (zero? n)
        (println 0)
        (let [dp (atom (mapv (fn [i] (mapv (fn [j] (if (= i j) 1 0)) (range n))) (range n)))]
          (doseq [len (range 2 (inc n)) i (range (inc (- n len)))]
            (let [j (+ i len -1)]
              (swap! dp assoc-in [i j]
                (if (= (nth s i) (nth s j))
                  (+ (if (> len 2) (nth (nth @dp (inc i)) (dec j)) 0) 2)
                  (max (nth (nth @dp (inc i)) j) (nth (nth @dp i) (dec j)))))))
          (println (nth (nth @dp 0) (dec n))))))))
