;; convex-hull-3d: randomized incremental 3D convex hull.
;; Fixed-seed shuffled insertion; each outside point deletes its
;; visible facets and cones over the horizon edges; new facets orient
;; against the initial tetrahedron's centroid (exact orientation
;; sums). Facet triples print sorted. General position required.
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
(defn orient3 [a b c d]
  (let [abx (- (nth b 0) (nth a 0)) aby (- (nth b 1) (nth a 1)) abz (- (nth b 2) (nth a 2))
        acx (- (nth c 0) (nth a 0)) acy (- (nth c 1) (nth a 1)) acz (- (nth c 2) (nth a 2))
        adx (- (nth d 0) (nth a 0)) ady (- (nth d 1) (nth a 1)) adz (- (nth d 2) (nth a 2))
        cx (- (* aby acz) (* abz acy)) cy (- (* abz acx) (* abx acz)) cz (- (* abx acy) (* aby acx))]
    (+ (* cx adx) (* cy ady) (* cz adz))))
(def LCG-A 6364136223846793005)
(def LCG-C 1442695040888963407)
(defn -main []
  (let [nums (mapv parse-token (tokenize (slurp *in*)))
        n (nth nums 0)
        pts (vec (for [i (range n)] [(nth nums (+ 1 (* 3 i))) (nth nums (+ 2 (* 3 i))) (nth nums (+ 3 (* 3 i)))]))
        ord (loop [i (dec n) st 199 ord (vec (range n))]
              (if (<= i 0) ord
                (let [st2 (unchecked-add (unchecked-multiply st LCG-A) LCG-C)
                      j (mod (unsigned-bit-shift-right st2 33) (inc i))]
                  (recur (dec i) st2 (assoc ord i (nth ord j) j (nth ord i))))))
        p0 (nth ord 0) p1 (nth ord 1) p2 (nth ord 2) p3 (nth ord 3)
        tt [p0 p1 p2 p3]
        init (mapv (fn [skip]
                     (let [v (vec (keep-indexed (fn [i x] (when (not= i skip) x)) tt))
                           [a b c] v]
                       (if (pos? (orient3 (nth pts a) (nth pts b) (nth pts c) (nth pts (nth tt skip))))
                         [a c b] [a b c]))) (range 4))
        orient-inner (fn [a b p]
                       (+ (orient3 (nth pts a) (nth pts b) (nth pts p) (nth pts p0))
                          (orient3 (nth pts a) (nth pts b) (nth pts p) (nth pts p1))
                          (orient3 (nth pts a) (nth pts b) (nth pts p) (nth pts p2))
                          (orient3 (nth pts a) (nth pts b) (nth pts p) (nth pts p3))))
        facets (loop [oi 4 facets init]
                 (if (>= oi n) facets
                   (let [p (nth ord oi)
                         vis (filterv (fn [f] (pos? (orient3 (nth pts (nth f 0)) (nth pts (nth f 1)) (nth pts (nth f 2)) (nth pts p)))) facets)]
                     (if (empty? vis) (recur (inc oi) facets)
                       (let [vset (set vis)
                             kept (filterv #(not (contains? vset %)) facets)
                             ec (reduce (fn [m [a b c]]
                                          (reduce (fn [m [u v]] (update m (if (< u v) [u v] [v u]) (fnil inc 0)))
                                                  m [[a b] [b c] [c a]])) {} vis)
                             newf (mapv (fn [[[u v] _]] (if (pos? (orient-inner u v p)) [v u p] [u v p]))
                                        (filter (fn [[_ c]] (= c 1)) ec))]
                         (recur (inc oi) (vec (concat kept newf))))))))]
    (let [out (vec (sort (map (fn [[a b c]] (vec (sort [a b c]))) facets)))]
      (println (count out))
      (doseq [[a b c] out] (println a b c)))))
(-main)
