;; bloom-filter: bit array + k double-hashed functions (Sedgewick Ch 3).
;; Line1: m k n q. Then n inserts, q queries. YES/NO each. Pure Clojure.

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

(def C1 -7046029254386353131)
(def C2 -4658895280553007687)

(defn slots [x m k]
  (let [h1 (unchecked-multiply x C1)
        h2 (bit-or (unchecked-add (unchecked-multiply x C2) 1) 1)]
    (mapv (fn [i] (int (mod (unchecked-add h1 (unchecked-multiply i h2)) m))) (range k))))

(defn solve [toks]
  (let [m (read-int (nth toks 0)) k (read-int (nth toks 1))
        n (read-int (nth toks 2)) q (read-int (nth toks 3))
        ins (mapv (fn [i] (read-int (nth toks (+ 4 i)))) (range n))
        queries (mapv (fn [i] (read-int (nth toks (+ 4 n i)))) (range q))
        bits (reduce (fn [b x] (reduce (fn [b2 s] (assoc b2 s 1)) b (slots x m k)))
                     (vec (repeat m 0)) ins)]
    (mapv (fn [x] (if (every? (fn [s] (= (nth bits s) 1)) (slots x m k)) "YES" "NO")) queries)))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
