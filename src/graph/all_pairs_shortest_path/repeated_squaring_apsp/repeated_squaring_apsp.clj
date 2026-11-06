;; repeated-squaring-apsp: all-pairs paths by min-plus squaring
;;
;; Faster-APSP: L^(2m) is the min-plus square of L^(m), doubling
;; to m >= n - 1. Emits the distance matrix with INF markers.
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
  (when (>= (count nums) 1)
    (let [n (nth nums 0)
          inf (bit-shift-left 1 58)
          L (atom (mapv (fn [i] (mapv (fn [j] (let [x (nth nums (+ 1 (* i n) j))]
                                                     (if (= x 999999) inf x))) (range n))) (range n)))]
      (loop [m 1]
        (when (< m (dec n))
          (let [N (mapv (fn [i] (mapv (fn [j] (loop [k 0 b inf]
                                                         (if (>= k n) b
                                                           (recur (inc k) (min b (+ (nth (nth @L i) k) (nth (nth @L k) j)))))))
                                        (range n))) (range n))]
            (reset! L N)
            (recur (* m 2)))))
      (when (= n 1) (swap! L assoc-in [0 0] 0))
      (doseq [i (range n)]
        (println (clojure.string/join " " (map #(if (>= % (quot inf 2)) "INF" (str %)) (nth @L i))))))))
