;; karp-min-mean-cycle: minimum mean-weight directed cycle
;;
;; Karp DP with exact integer ratios: F[k][v] walks, then the min
;; over v of max over k of (F[n][v]-F[k][v])/(n-k) via exact cross
;; products. Emits the reduced fraction or NO CYCLE.
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
  (let [done (reduce (fn [acc ch]
                       (if (contains? separators ch)
                         (if (empty? (:current acc))
                           acc
                           {:tokens (conj (:tokens acc) (apply str (:current acc)))
                             :current []})
                         (update acc :current conj ch)))
                     {:tokens [] :current []}
                     s)]
    (if (empty? (:current done))
      (:tokens done)
      (conj (:tokens done) (apply str (:current done))))))

(def min-long
  "Most negative long value: 1 shifted into the sign bit."
  (bit-shift-left 1 63))

(def max-long
  "Largest long value: every bit below the sign bit set."
  (bit-not min-long))

(defn parse-token
  "Parse tok as a decimal long integer with optional sign. Returns
   nil when tok is malformed or out of long range; the bounds check
   runs before each multiply-and-add step so no step can overflow."
  [tok]
  (let [chs (seq tok)]
    (when (seq chs)
      (let [head (first chs)
            neg? (= head \-)
            rest-chs (if (or neg? (= head \+)) (rest chs) chs)]
        (when (seq rest-chs)
          (if neg?
            (reduce (fn [acc ch]
                      (when (some? acc)
                        (let [d (get digit-values ch)]
                          (when (and (some? d)
                                     (>= acc (quot (+ min-long d) 10)))
                            (- (* acc 10) d)))))
                    0 rest-chs)
            (reduce (fn [acc ch]
                      (when (some? acc)
                        (let [d (get digit-values ch)]
                          (when (and (some? d)
                                     (<= acc (quot (- max-long d) 10)))
                            (+ (* acc 10) d)))))
                    0 rest-chs)))))))

(defn read-longs
  "Read all of stdin as a vector of longs."
  []
  (mapv parse-token (tokenize (slurp *in*))))


(let [nums (read-longs)]
  (when (>= (count nums) 2)
    (let [n (nth nums 0) m (nth nums 1)
          es (mapv (fn [i] [(nth nums (+ 2 (* i 3))) (nth nums (+ 3 (* i 3))) (nth nums (+ 4 (* i 3)))]) (range m))
          inf (bit-shift-left 1 60)
          F (atom (mapv (fn [k] (mapv (fn [_] (if (zero? k) 0 inf)) (range n))) (range (inc n))))]
      (doseq [k (range 1 (inc n)) [u v w] es]
        (when (< (+ (nth (nth @F (dec k)) u) w) (nth (nth @F k) v))
          (swap! F assoc-in [k v] (+ (nth (nth @F (dec k)) u) w))))
      (let [best (atom nil)]
        (doseq [v (range n)]
          (when (< (nth (nth @F n) v) (quot inf 2))
            (let [bv (loop [k 0 acc nil]
                       (if (>= k n) acc
                         (if (>= (nth (nth @F k) v) (quot inf 2))
                           (recur (inc k) acc)
                           (let [num (- (nth (nth @F n) v) (nth (nth @F k) v)) den (- n k)]
                             (recur (inc k) (if (or (nil? acc) (> (* num (nth acc 1)) (* (nth acc 0) den)))
                                                     [num den] acc))))))]
              (when (some? bv)
                (when (or (nil? @best) (< (* (nth bv 0) (nth @best 1)) (* (nth @best 0) (nth bv 1))))
                  (reset! best bv))))))
        (if (nil? @best)
          (println "NO CYCLE")
          (let [[bn bd] @best
                g (loop [a (if (neg? bn) (- bn) bn) b bd] (if (zero? b) a (recur b (mod a b))))]
            (println (str (quot bn g) "/" (quot bd g)))))))))
