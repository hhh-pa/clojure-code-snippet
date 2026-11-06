;; subset-sum: subset sum and partition via knapsack DP
;;
;; Bitset-style knapsack DP answers the target query; the partition
;; query reuses it on half the total when even. Emits YES/NO lines.
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
    (let [n (nth nums 0) target (nth nums 1)
          vals (mapv #(nth nums (+ 2 %)) (range n))
          poss (fn [t]
                 (if (neg? t) false
                   (let [dp (loop [vs vals acc (assoc (vec (repeat (inc t) false)) 0 true)]
                               (if (empty? vs) acc
                                 (let [x (first vs)]
                                   (if (or (neg? x) (> x t))
                                     (recur (rest vs) acc)
                                     (recur (rest vs)
                                       (loop [s t a2 acc]
                                         (if (< s x) a2
                                           (recur (dec s) (if (nth a2 (- s x)) (assoc a2 s true) a2)))))))))]
                     (nth dp t))))
          tot (reduce + 0 vals)]
      (println (if (poss target) "YES" "NO"))
      (println (if (and (even? tot) (poss (quot tot 2))) "YES" "NO")))))
