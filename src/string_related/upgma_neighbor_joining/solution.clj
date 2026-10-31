;; upgma-neighbor-joining: evolutionary tree reconstruction from a
;; distance matrix. UPGMA merges closest clusters (size-weighted
;; average linkage); neighbor-joining merges the minimum-Q pair with
;; Saitou-Nei updates. Output: UPGMA merges then NJ merges, one pair
;; per line; ties resolve to the lowest pair. Cluster ids grow past n.
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
(defn active-pairs [on nid]
  (for [i (range nid) :when (nth on i) j (range (inc i) nid) :when (nth on j)] [i j]))
(defn upgma [d0 n]
  (let [N (* 2 n)]
    (loop [d d0 sz (vec (repeat N 1))
           on (vec (concat (repeat n true) (repeat n false)))
           cnt n nid n out []]
      (if (<= cnt 1) out
        (let [pairs (active-pairs on nid)
              [bi bj] (reduce (fn [[bi bj] [i j]]
                                (if (< (nth (nth d i) j) (nth (nth d bi) bj)) [i j] [bi bj]))
                              (first pairs) (rest pairs))
              d2 (reduce (fn [d k]
                           (if (and (nth on k) (not= k bi) (not= k bj))
                             (let [v (/ (+ (* (nth sz bi) (nth (nth d bi) k))
                                           (* (nth sz bj) (nth (nth d bj) k)))
                                        (+ (nth sz bi) (nth sz bj)))]
                               (-> d (assoc-in [nid k] v) (assoc-in [k nid] v)))
                             d))
                         d (range nid))]
          (recur d2 (assoc sz nid (+ (nth sz bi) (nth sz bj)))
                 (-> on (assoc bi false) (assoc bj false) (assoc nid true))
                 (dec cnt) (inc nid) (conj out [bi bj])))))))
(defn neighbor-joining [d0 n]
  (let [N (* 2 n)]
    (loop [d d0 on (vec (concat (repeat n true) (repeat n false))) cnt n nid n out []]
      (if (<= cnt 2)
        (let [act (filterv #(nth on %) (range nid))]
          (conj out [(nth act 0) (nth act 1)]))
        (let [R (mapv (fn [i] (if (nth on i)
                                (reduce + (map (fn [k] (nth (nth d i) k)) (filter #(nth on %) (range nid))))
                                0.0)) (range N))
              pairs (active-pairs on nid)
              q (fn [[i j]] (- (* (- cnt 2) (nth (nth d i) j)) (nth R i) (nth R j)))
              [bi bj] (reduce (fn [p1 p2] (if (< (q p2) (q p1)) p2 p1)) (first pairs) (rest pairs))
              d2 (reduce (fn [d k]
                           (if (and (nth on k) (not= k bi) (not= k bj))
                             (let [v (/ (- (+ (nth (nth d bi) k) (nth (nth d bj) k)) (nth (nth d bi) bj)) 2.0)]
                               (-> d (assoc-in [nid k] v) (assoc-in [k nid] v)))
                             d))
                         d (range nid))]
          (recur d2 (-> on (assoc bi false) (assoc bj false) (assoc nid true))
                 (dec cnt) (inc nid) (conj out [bi bj])))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))
        n (parse-token (nth toks 0)) N (* 2 n)
        vals (mapv double (map parse-token (take (* n n) (drop 1 toks))))
        d0 (vec (for [i (range N)] (vec (for [j (range N)]
                                                  (if (and (< i n) (< j n)) (nth vals (+ (* i n) j)) 0.0)))))]
    (doseq [[a b] (concat (upgma d0 n) (neighbor-joining d0 n))]
      (println a b))))
(-main)
