;; dynamic-array: doubling/halving amortized table (CLRS 16.4).
;; Push doubles capacity when full; pop halves when quarter full.
;; Ops: 1 x push, 2 pop, 3 i get, 4 size+capacity. Pure Clojure.

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

(defn solve [toks]
  (let [q (read-int (nth toks 0))]
    (loop [k 0 i 1 a [] cap 1 out []]
      (if (>= k q) out
        (let [op (read-int (nth toks i))]
          (cond
            (= op 1) (let [x (read-int (nth toks (+ i 1)))
                           cap2 (if (= (count a) cap) (* cap 2) cap)]
                       (recur (+ k 1) (+ i 2) (conj a x) cap2 out))
            (= op 2) (if (empty? a)
                       (recur (+ k 1) (+ i 1) a cap (conj out "EMPTY"))
                       (let [v (peek a) a2 (pop a)
                             cap2 (if (and (seq a2) (<= (* (count a2) 4) cap) (> cap 1)) (quot cap 2) cap)]
                         (recur (+ k 1) (+ i 1) a2 cap2 (conj out (str v)))))
            (= op 3) (let [j (read-int (nth toks (+ i 1)))]
                       (recur (+ k 1) (+ i 2) a cap
                              (conj out (if (and (>= j 0) (< j (count a))) (str (nth a j)) "NONE"))))
            (= op 4) (recur (+ k 1) (+ i 1) a cap (conj out (str (count a) " " cap)))
            :else (recur (+ k 1) (+ i 1) a cap out)))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
