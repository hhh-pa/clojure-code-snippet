;; hash-table-separate-chaining: linked bucket chains (CLRS 11.2).
;; Line1: m q. Ops: 1 k v put, 2 k get, 3 k delete, 4 bucket sizes.
;; Pure Clojure.

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
  (let [m (read-int (nth toks 0)) q (read-int (nth toks 1))]
    (loop [k 0 i 2 t (vec (repeat m [])) out []]
      (if (>= k q) out
        (let [op (read-int (nth toks i))]
          (cond
            (= op 1) (let [kk (read-int (nth toks (+ i 1))) v (read-int (nth toks (+ i 2)))
                           b (mod kk m) bucket (nth t b)
                           idx (loop [j 0] (cond (>= j (count bucket)) -1
                                                (= (nth (nth bucket j) 0) kk) j
                                                :else (recur (+ j 1))))
                           t2 (if (>= idx 0)
                                (assoc t b (assoc bucket idx [kk v]))
                                (assoc t b (conj bucket [kk v])))]
                       (recur (+ k 1) (+ i 3) t2 out))
            (= op 2) (let [kk (read-int (nth toks (+ i 1))) b (mod kk m)
                           hit (loop [j 0] (cond (>= j (count (nth t b))) nil
                                                (= (nth (nth (nth t b) j) 0) kk) (nth (nth (nth t b) j) 1)
                                                :else (recur (+ j 1))))]
                       (recur (+ k 1) (+ i 2) t (conj out (if (nil? hit) "NONE" (str hit)))))
            (= op 3) (let [kk (read-int (nth toks (+ i 1))) b (mod kk m) bucket (nth t b)
                           idx (loop [j 0] (cond (>= j (count bucket)) -1
                                                (= (nth (nth bucket j) 0) kk) j
                                                :else (recur (+ j 1))))]
                       (if (>= idx 0)
                         (recur (+ k 1) (+ i 2) (assoc t b (vec (concat (subvec bucket 0 idx) (subvec bucket (+ idx 1))))) (conj out "OK"))
                         (recur (+ k 1) (+ i 2) t (conj out "NONE"))))
            (= op 4) (recur (+ k 1) (+ i 1) t
                            (conj out (apply str (interpose " " (map (comp str count) t)))))
            :else (recur (+ k 1) (+ i 1) t out)))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
