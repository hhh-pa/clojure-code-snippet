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
