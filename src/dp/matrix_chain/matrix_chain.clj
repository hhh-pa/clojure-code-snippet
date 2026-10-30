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
