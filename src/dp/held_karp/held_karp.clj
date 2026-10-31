;; held-karp: TSP bitmask dynamic programming
;;
;; Held-Karp over subsets: dp[mask][u] is the cheapest path from 0
;; visiting exactly mask and ending at u. Emits the optimal tour cost.
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
  (when (>= (count nums) 1)
    (let [n (nth nums 0)
          d (mapv (fn [i] (mapv (fn [j] (nth nums (+ 1 (* i n) j))) (range n))) (range n))]
      (if (<= n 1)
        (println 0)
        (let [inf (bit-shift-left 1 60) N (bit-shift-left 1 n)
              dp (atom (mapv (fn [_] (mapv (fn [_] inf) (range n))) (range N)))]
          (swap! dp assoc-in [1 0] 0)
          (doseq [mask (range N) :when (odd? mask)
                  u (range n) :when (and (pos? (bit-and mask (bit-shift-left 1 u)))
                                              (< (nth (nth @dp mask) u) inf))]
            (doseq [vv (range 1 n) :when (zero? (bit-and mask (bit-shift-left 1 vv)))]
              (let [nm (bit-or mask (bit-shift-left 1 vv))
                    c (+ (nth (nth @dp mask) u) (nth (nth d u) vv))]
                (when (< c (nth (nth @dp nm) vv))
                  (swap! dp assoc-in [nm vv] c)))))
          (let [full (dec N)]
            (println (reduce min (map (fn [u] (+ (nth (nth @dp full) u) (nth (nth d u) 0))) (range n))))))))))
