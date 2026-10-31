;; buddy-allocator: power-of-2 page frame allocator (Tanenbaum MOS).
;; Line1: U q (total 2^U). Ops: 1 s alloc (addr/FAIL),
;; 2 a free (OK/FAIL), 3 free counts per order 0..U. Pure Clojure.

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

(defn pow2 [e] (bit-shift-left 1 e))

(defn solve [toks]
  (let [U (read-int (nth toks 0)) q (read-int (nth toks 1))]
    (loop [k 0 i 2 free (assoc (vec (repeat (+ U 1) (sorted-set))) U (sorted-set 0)) alloc {} out []]
      (if (>= k q) out
        (let [op (read-int (nth toks i))]
          (cond
            (= op 1) (let [s (read-int (nth toks (+ i 1)))]
                       (if (< s 1) (recur (+ k 1) (+ i 2) free alloc (conj out "FAIL"))
                         (let [need (loop [nd 0] (if (or (> nd U) (>= (pow2 nd) s)) nd (recur (+ nd 1))))
                               found (loop [o need] (cond (> o U) -1
                                                         (seq (nth free o)) o
                                                         :else (recur (+ o 1))))]
                           (if (or (> need U) (= found -1))
                             (recur (+ k 1) (+ i 2) free alloc (conj out "FAIL"))
                             (let [at (first (nth free found))
                                   free2 (loop [o found fr (assoc free found (disj (nth free found) at))]
                                           (if (<= o need) fr
                                             (recur (- o 1) (assoc fr (- o 1) (conj (nth fr (- o 1)) (+ at (pow2 (- o 1))))))))]
                               (recur (+ k 1) (+ i 2) free2 (assoc alloc at need) (conj out (str at))))))))
            (= op 2) (let [a (read-int (nth toks (+ i 1)))]
                       (if (not (contains? alloc a))
                         (recur (+ k 1) (+ i 2) free alloc (conj out "FAIL"))
                         (let [o (get alloc a)
                               [free2 at2 o2] (loop [fr free at a oo o]
                                                (if (>= oo U) [fr at oo]
                                                  (let [buddy (bit-xor at (pow2 oo))]
                                                    (if (contains? (nth fr oo) buddy)
                                                      (recur (assoc fr oo (disj (nth fr oo) buddy))
                                                             (min at buddy) (+ oo 1))
                                                      [fr at oo]))))]
                           (recur (+ k 1) (+ i 2) (assoc free2 o2 (conj (nth free2 o2) at2))
                                  (dissoc alloc a) (conj out "OK")))))
            (= op 3) (recur (+ k 1) (+ i 1) free alloc
                            (conj out (apply str (interpose " " (map (comp str count) free)))))
            :else (recur (+ k 1) (+ i 1) free alloc out)))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
