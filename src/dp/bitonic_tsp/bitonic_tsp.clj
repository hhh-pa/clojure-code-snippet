;; bitonic-tsp: bitonic Euclidean tour DP
;;
;; Bitonic DP over x-sorted points with hypot distances: extend
;; the chain or close the gap from the best predecessor. Emits the
;; optimal tour length.
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


(defn parse-decimal
  "Parse tok as a decimal floating value with Double/parseDouble so the
   result is correctly rounded exactly like the C++ and Java drivers."
  [tok]
  (Double/parseDouble tok))

(defn read-doubles-after
  "Read stdin tokens, skipping the first k, as doubles."
  [k]
  (mapv parse-decimal (subvec (vec (tokenize (slurp *in*))) k)))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks)
    (let [n (parse-token (nth toks 0))
          xs (mapv #(parse-decimal (nth toks (+ 1 (* % 2)))) (range n))
          ys (mapv #(parse-decimal (nth toks (+ 2 (* % 2)))) (range n))
          out (fn [x] (println (String/format java.util.Locale/US "%.6f" (to-array [(double x)]))))
          dist (fn [a b] (Math/hypot (- (nth xs a) (nth xs b)) (- (nth ys a) (nth ys b))))]
      (cond (<= n 1) (out 0.0)
            (= n 2) (out (* 2 (dist 0 1)))
            :else (let [dp (atom (mapv (fn [_] (mapv (fn [_] 1e100) (range n))) (range n)))]
                    (swap! dp assoc-in [0 1] (dist 0 1))
                    (doseq [j (range 2 n)]
                      (doseq [i (range (dec j))]
                        (swap! dp assoc-in [i j] (+ (nth (nth @dp i) (dec j)) (dist (dec j) j))))
                      (let [best (loop [k 0 acc 1e100]
                                   (if (>= k (dec j)) acc
                                     (recur (inc k) (min acc (+ (nth (nth @dp k) (dec j)) (dist k j))))))]
                        (swap! dp assoc-in [(dec j) j] best)))
                    (out (+ (nth (nth @dp (- n 2)) (dec n)) (dist (- n 2) (dec n)))))))))
