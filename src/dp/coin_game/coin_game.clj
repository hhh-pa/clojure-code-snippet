;; coin-game: optimal two-player coin row strategy
;;
;; Interval DP: each end pairs with the worse of the two replies
;; the opponent leaves. Emits the first player optimum.
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


(let [nums (read-longs)]
  (if (empty? nums)
    (println 0)
    (let [n (nth nums 0)
          cv (mapv #(nth nums (inc %)) (range n))]
      (if (zero? n)
        (println 0)
        (let [dp (atom (mapv (fn [i] (mapv (fn [j] (if (= i j) (nth cv i) 0)) (range n))) (range n)))]
          (doseq [L (range 2 (inc n)) i (range (inc (- n L)))]
            (let [j (+ i L -1)
                  a (if (<= (+ i 2) j) (nth (nth @dp (+ i 2)) j) 0)
                  b (if (<= (+ i 1) (- j 1)) (nth (nth @dp (+ i 1)) (- j 1)) 0)
                  c (if (<= i (- j 2)) (nth (nth @dp i) (- j 2)) 0)]
              (swap! dp assoc-in [i j]
                (max (+ (nth cv i) (min a b)) (+ (nth cv j) (min b c))))))
          (println (nth (nth @dp 0) (dec n))))))))
