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
