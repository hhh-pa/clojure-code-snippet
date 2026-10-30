;; priority-search-tree: 3-sided orthogonal range queries.
;; Each node stores its subtree's maximum-y point (heap on y) while
;; the remaining points split by x-order (BST on x). Queries report
;; points in [x1,x2] with y above y0, pruning subtrees by x-range
;; overlap and child-root y. Matches print sorted as x,y pairs.
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
(defn build-pst [tree pts ids]
  (let [best (reduce (fn [b i] (if (> (nth (nth pts (nth ids i)) 1) (nth (nth pts (nth ids b)) 1)) i b))
                     0 (range (count ids)))
        pid (nth ids best)
        left-ids (vec (keep-indexed (fn [i v] (when (< i best) v)) ids))
        right-ids (vec (keep-indexed (fn [i v] (when (> i best) v)) ids))
        id (count tree)
        [t1 L] (if (seq left-ids) (build-pst tree pts left-ids) [tree -1])
        [t2 R] (if (seq right-ids) (build-pst t1 pts right-ids) [t1 -1])
        p (nth pts pid)
        xmin (apply min (nth p 0) (concat (when (>= L 0) [(nth (nth t2 L) 1)])
                                          (when (>= R 0) [(nth (nth t2 R) 1)])))
        xmax (apply max (nth p 0) (concat (when (>= L 0) [(nth (nth t2 L) 2)])
                                          (when (>= R 0) [(nth (nth t2 R) 2)])))]
    [(conj t2 [p xmin xmax L R]) (count t2)]))
(defn query-pst [tree v x1 x2 y0]
  (if (neg? v) []
    (let [[p xmin xmax L R] (nth tree v)
          here (if (and (>= (nth p 1) y0) (<= x1 (nth p 0) x2)) [p] [])
          lres (if (and (>= L 0)
                        (let [[_ lmin lmax _ _] (nth tree L)]
                          (and (>= lmax x1) (<= lmin x2) (>= (nth (nth (nth tree L) 0) 1) y0))))
                   (query-pst tree L x1 x2 y0) [])
          rres (if (and (>= R 0)
                        (let [[_ rmin rmax _ _] (nth tree R)]
                          (and (>= rmax x1) (<= rmin x2) (>= (nth (nth (nth tree R) 0) 1) y0))))
                   (query-pst tree R x1 x2 y0) [])]
      (concat here lres rres))))
(defn -main []
  (let [nums (mapv parse-token (tokenize (slurp *in*)))
        n (nth nums 0)
        pts (vec (for [i (range n)] [(nth nums (+ 1 (* 2 i))) (nth nums (+ 2 (* 2 i)))]))
        xs (vec (sort-by (fn [i] [(nth (nth pts i) 0) (nth (nth pts i) 1)]) (range n)))
        [tree root] (build-pst [] pts xs)
        base (+ 1 (* 2 n))
        q (nth nums base)]
    (loop [i 0]
      (when (< i q)
        (let [x1 (nth nums (+ base 1 (* 3 i))) x2 (nth nums (+ base 2 (* 3 i))) y0 (nth nums (+ base 3 (* 3 i)))
              found (vec (sort (query-pst tree root x1 x2 y0)))]
          (println (count found))
          (when (seq found)
            (println (apply str (interpose " " (map (fn [[x y]] (str x "," y)) found))))))
        (recur (inc i))))))
(-main)
