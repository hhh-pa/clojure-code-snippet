;; byers-waterman: enumerate all alignments within Delta of optimal.
;; Forward and reverse edit tables; DFS tracks prefix cost g and
;; prunes g+R above OPT+Delta (exact: completions cost at least R).
;; Paths print sorted lexicographically. Input: A B Delta. Output:
;; count, then aligned pairs.
(def digit-values (zipmap "0123456789" (range)))
(def separators #{\space \tab \newline \return \formfeed \u000B})
(defn tokenize [s]
  (let [done (reduce (fn [{:keys [tokens current]} ch]
                       (if (contains? separators ch)
                         (if (empty? current) {:tokens tokens :current []}
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
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (>= acc (quot (+ min-long d) 10))) (- (* acc 10) d))))) 0 rest-chs)
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs)))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))
        a (vec (nth toks 0)) b (vec (nth toks 1)) dd (parse-token (nth toks 2))
        n (count a) m (count b) w (inc m)
        idx (fn [i j] (+ (* i w) j))
        fwd (reduce (fn [dp i]
                      (reduce (fn [dp j]
                                (assoc dp (idx i j)
                                       (min (+ (nth dp (idx (dec i) j)) 1)
                                            (+ (nth dp (idx i (dec j))) 1)
                                            (+ (nth dp (idx (dec i) (dec j)))
                                               (if (= (nth a (dec i)) (nth b (dec j))) 0 1)))))
                              dp (range 1 (inc m))))
                    (vec (for [i (range (inc n)) j (range (inc m))] (cond (zero? j) i (zero? i) j :else 0)))
                    (range 1 (inc n)))
        rev (reduce (fn [dp i]
                      (reduce (fn [dp j]
                                (assoc dp (idx i j)
                                       (min (+ (nth dp (idx (inc i) j)) 1)
                                            (+ (nth dp (idx i (inc j))) 1)
                                            (+ (nth dp (idx (inc i) (inc j)))
                                               (if (= (nth a i) (nth b j)) 0 1)))))
                              dp (range (dec m) -1 -1)))
                    (vec (for [i (range (inc n)) j (range (inc m))] (cond (= j m) (- n i) (= i n) (- m j) :else 0)))
                    (range (dec n) -1 -1))
        opt (nth fwd (idx n m))
        paths (loop [st [[0 0 0 0]] ca [] cb [] out []]
                (if (empty? st) out
                  (let [[i j mv g] (peek st) rst (pop st)]
                    (cond
                      (and (= i n) (= j m))
                      (recur rst (if (seq ca) (pop ca) ca) (if (seq cb) (pop cb) cb)
                             (conj out [(apply str ca) (apply str cb)]))
                      (>= mv 3)
                      (recur rst (if (pos? (+ i j)) (pop ca) ca) (if (pos? (+ i j)) (pop cb) cb) out)
                      :else
                      (let [up (conj rst [i j (inc mv) g])]
                        (cond
                          (zero? mv)
                          (if (and (< i n) (< j m))
                            (let [dc (if (= (nth a i) (nth b j)) 0 1)]
                              (if (<= (+ g dc (nth rev (idx (inc i) (inc j)))) (+ opt dd))
                                (recur (conj up [(inc i) (inc j) 0 (+ g dc)])
                                       (conj ca (nth a i)) (conj cb (nth b j)) out)
                                (recur up ca cb out)))
                            (recur up ca cb out))
                          (= mv 1)
                          (if (and (< i n) (<= (+ g 1 (nth rev (idx (inc i) j))) (+ opt dd)))
                            (recur (conj up [(inc i) j 0 (inc g)])
                                   (conj ca (nth a i)) (conj cb \-) out)
                            (recur up ca cb out))
                          :else
                          (if (and (< j m) (<= (+ g 1 (nth rev (idx i (inc j)))) (+ opt dd)))
                            (recur (conj up [i (inc j) 0 (inc g)])
                                   (conj ca \-) (conj cb (nth b j)) out)
                            (recur up ca cb out))))))))]
    (println (count paths))
    (doseq [[x y] (sort-by (juxt first second) paths)]
      (println x) (println y))))
(-main)
