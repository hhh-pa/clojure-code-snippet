;; josephus: (n,m)-elimination via order-statistic Fenwick tree (CLRS 17).
;; O(n log n). Input: n m. Output: elimination order, 1-based. Pure Clojure.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize [s]
  (let [done (reduce (fn [{:keys [tokens current]} ch]
                       (if (contains? separators ch)
                         (if (empty? current)
                           {:tokens tokens :current []}
                           {:tokens (conj tokens (apply str current)) :current []})
                         {:tokens tokens :current (conj current ch)}))
                     {:tokens [] :current []} s)]
    (if (empty? (:current done)) (:tokens done)
        (conj (:tokens done) (apply str (:current done))))))

(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))

(defn parse-token [tok]
  (let [chs (seq tok)]
    (when (seq chs)
      (let [head (first chs) neg? (= head \-) rest-chs (if (or neg? (= head \+)) (rest chs) chs)]
        (when (seq rest-chs)
          (if neg?
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)]
              (when (and (some? d) (>= acc (quot (+ min-long d) 10))) (- (* acc 10) d))))) 0 rest-chs)
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)]
              (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs)))))))

(defn read-int [tok] (parse-token tok))

(defn bit-add [bit n i d]
  (loop [bit bit i i]
    (if (> i n) bit
      (recur (assoc bit i (+ (nth bit i) d)) (+ i (bit-and i (- i)))))))

(defn bit-kth [bit n k]
  (let [step (loop [s 1] (if (<= (* s 2) n) (recur (* s 2)) s))]
    (loop [i 0 st step k k]
      (if (zero? st) (+ i 1)
        (let [j (+ i st)]
          (if (and (<= j n) (< (nth bit j) k))
            (recur j (quot st 2) (- k (nth bit j)))
            (recur i (quot st 2) k)))))))

(defn solve [toks]
  (let [n (read-int (nth toks 0)) m (read-int (nth toks 1))
        bit0 (loop [i 1 b (vec (repeat (+ n 1) 0))]
               (if (> i n) b (recur (+ i 1) (bit-add b n i 1))))]
    (loop [pos 0 rem n bit bit0 out []]
      (if (zero? rem) out
        (let [p (mod (+ pos m -1) rem)
              idx (bit-kth bit n (+ p 1))]
          (recur p (- rem 1) (bit-add bit n idx -1) (conj out idx)))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks)
    (println (apply str (interpose " " (map str (solve toks)))))))
