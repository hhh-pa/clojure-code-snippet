;; fractional-knapsack: greedy by value density
;;
;; Sorts items by value/weight with exact cross-multiplied compare
;; (ties by index), then takes whole items and one fraction. Emits the
;; optimum value with six decimals.
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
  (when (>= (count nums) 2)
    (let [n (nth nums 0) cap (nth nums 1)
          items (mapv (fn [i] [(nth nums (+ 2 (* i 2))) (nth nums (+ 3 (* i 2)))]) (range n))
          cmp (fn [a b]
                (let [l (* (nth (nth items a) 0) (nth (nth items b) 1))
                      r (* (nth (nth items b) 0) (nth (nth items a) 1))]
                  (cond (> l r) -1 (< l r) 1 (< a b) -1 (> a b) 1 :else 0)))
          o (vec (sort cmp (range n)))
          tot (loop [js o rem (double cap) acc 0.0]
                (if (or (empty? js) (<= rem 0)) acc
                  (let [i (first js) vv (double (nth (nth items i) 0)) ww (double (nth (nth items i) 1))]
                    (if (<= ww rem)
                      (recur (rest js) (- rem ww) (+ acc vv))
                      (recur (rest js) 0.0 (+ acc (* vv (/ rem ww))))))))]
      (println (String/format java.util.Locale/US "%.6f" (to-array [(double tot)]))))))
