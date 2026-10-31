;; four-russians: edit distance via 2x2 block decomposition and lookup table.
;; The DP table is tiled into 2x2 blocks; each block's output border
;; offsets are a pure function of its input border offsets and the two
;; 2-mers, memoized in a transition table (offsets are translation
;; invariant under unit costs). Odd fringe strips use the direct
;; recurrence. Input: two strings. Output: Levenshtein distance.
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
(defn solve-block [memo a0 a1 b0 b1 t0 t1 l0 l1]
  (let [k [a0 a1 b0 b1 t0 t1 l0 l1]]
    (if-let [v (get @memo k)] v
      (let [t1v t0 t2v (+ t0 t1) l1v l0 l2v (+ l0 l1)
            x11 (min (+ t1v 1) (+ l1v 1) (if (= a0 b0) 0 1))
            x12 (min (+ t2v 1) (+ x11 1) (+ t1v (if (= a0 b1) 0 1)))
            x21 (min (+ x11 1) (+ l2v 1) (+ l1v (if (= a1 b0) 0 1)))
            x22 (min (+ x12 1) (+ x21 1) (+ x11 (if (= a1 b1) 0 1)))
            v [(- x21 l2v) (- x22 x21) (- x12 t2v) (- x22 x12)]]
        (swap! memo assoc k v) v))))
(defn -main []
  (let [toks (tokenize (slurp *in*)) a (vec (nth toks 0)) b (vec (nth toks 1))
        n (count a) m (count b) w (inc m)
        idx (fn [i j] (+ (* i w) j))
        dp0 (vec (for [i (range (inc n)) j (range (inc m))] (cond (zero? j) i (zero? i) j :else 0)))
        memo (atom {})
        cell (fn [dp i j] (min (+ (nth dp (idx (dec i) j)) 1) (+ (nth dp (idx i (dec j))) 1)
                               (+ (nth dp (idx (dec i) (dec j))) (if (= (nth a (dec i)) (nth b (dec j))) 0 1))))
        dp1 (reduce (fn [dp [bi bj]]
                      (let [i0 (* 2 bi) j0 (* 2 bj)
                            t0 (- (nth dp (idx i0 (inc j0))) (nth dp (idx i0 j0)))
                            t1 (- (nth dp (idx i0 (+ j0 2))) (nth dp (idx i0 (inc j0))))
                            l0 (- (nth dp (idx (inc i0) j0)) (nth dp (idx i0 j0)))
                            l1 (- (nth dp (idx (+ i0 2) j0)) (nth dp (idx (inc i0) j0)))
                            v (solve-block memo (nth a i0) (nth a (inc i0)) (nth b j0) (nth b (inc j0)) t0 t1 l0 l1)]
                        (-> dp
                            (assoc (idx (+ i0 2) (inc j0)) (+ (nth dp (idx (+ i0 2) j0)) (nth v 0)))
                            (assoc (idx (+ i0 2) (+ j0 2)) (+ (nth dp (idx (+ i0 2) j0)) (nth v 0) (nth v 1)))
                            (assoc (idx (inc i0) (+ j0 2)) (+ (nth dp (idx i0 (+ j0 2))) (nth v 2))))))
                    dp0 (for [bi (range (quot n 2)) bj (range (quot m 2))] [bi bj]))
        nodd (odd? n) modd (odd? m)
        dp2 (if (and nodd modd)
              (let [dpr (loop [j 1 dp dp1] (if (>= j m) dp (recur (inc j) (assoc dp (idx n j) (cell dp n j)))))]
                (loop [i 1 dp dpr] (if (> i n) dp (recur (inc i) (assoc dp (idx i m) (cell dp i m))))))
              (let [dpc (if modd (loop [i 1 dp dp1] (if (> i n) dp (recur (inc i) (assoc dp (idx i m) (cell dp i m))))) dp1)]
                (if nodd (loop [j 1 dp dpc] (if (> j m) dp (recur (inc j) (assoc dp (idx n j) (cell dp n j))))) dpc)))]
    (println (nth dp2 (idx n m)))))
(-main)
