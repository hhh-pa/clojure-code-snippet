;; matrix-chain: optimal parenthesization order
;;
;; Interval DP over chain lengths: m[i][j] is the cheapest split
;; k plus dimensions product. Emits the minimum scalar count.
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
  (when (seq nums)
    (let [n (nth nums 0)
          d (mapv #(nth nums (inc %)) (range (inc n)))]
      (if (<= n 1)
        (println 0)
        (let [m (atom (mapv (fn [_] (mapv (fn [_] 0) (range n))) (range n)))]
          (doseq [l (range 2 (inc n)) i (range (inc (- n l)))]
            (let [j (+ i l -1)]
              (swap! m assoc-in [i j] max-long)
              (doseq [k (range i j)]
                (let [c (+ (nth (nth @m i) k) (nth (nth @m (inc k)) j)
                           (* (nth d i) (nth d (inc k)) (nth d (inc j))))]
                  (when (< c (nth (nth @m i) j))
                    (swap! m assoc-in [i j] c))))))
          (println (nth (nth @m 0) (dec n))))))))
