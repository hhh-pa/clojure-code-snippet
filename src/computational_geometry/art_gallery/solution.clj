;; art-gallery: floor(n/3) guards via ear-clipping triangulation and
;; 3-coloring of the dual tree. Lowest scan-order ear clips first;
;; BFS from triangle zero colors neighbors through shared edges; the
;; smallest color class (ties by sorted list) guards the polygon.
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
(defn cross [a b c]
  (- (* (- (nth b 0) (nth a 0)) (- (nth c 1) (nth a 1)))
     (* (- (nth b 1) (nth a 1)) (- (nth c 0) (nth a 0)))))
(defn -main []
  (let [nums (mapv parse-token (tokenize (slurp *in*)))
        n (nth nums 0)
        pin (vec (for [i (range n)] [(nth nums (+ 1 (* 2 i))) (nth nums (+ 2 (* 2 i)))]))
        area2 (reduce + (map (fn [i] (let [a (nth pin i) b (nth pin (mod (inc i) n))]
                                          (- (* (nth a 0) (nth b 1)) (* (nth a 1) (nth b 0))))) (range n)))
        pts (if (neg? area2) (vec (reverse pin)) pin)
        w2o (if (neg? area2) (vec (map (fn [i] (- n 1 i)) (range n))) (vec (range n)))
        tris (loop [poly (vec (range n)) tris []]
               (if (<= (count poly) 3) (conj tris poly)
                 (let [m (count poly)
                       found (first (for [k (range m)
                                          :let [a (nth poly (mod (dec k) m)) b (nth poly k) c (nth poly (mod (inc k) m))]
                                          :when (and (pos? (cross (nth pts a) (nth pts b) (nth pts c)))
                                                     (not-any? (fn [v] (and (not= v a) (not= v b) (not= v c)
                                                                            (let [p (nth pts v)]
                                                                              (and (pos? (cross (nth pts a) (nth pts b) p))
                                                                                   (pos? (cross (nth pts b) (nth pts c) p))
                                                                                   (pos? (cross (nth pts c) (nth pts a) p)))))) poly))]
                                      k))
                       a (nth poly (mod (dec found) m)) b (nth poly found) c (nth poly (mod (inc found) m))]
                   (recur (vec (concat (subvec poly 0 found) (subvec poly (inc found)))) (conj tris [a b c])))))
        emap (reduce (fn [em [ti tr]] (reduce (fn [em e] (let [u (nth tr e) v (nth tr (mod (inc e) 3))
                                                                  k (if (< u v) [u v] [v u])]
                                                              (update em k (fnil conj []) ti))) em (range 3)))
                     {} (map-indexed vector tris))
        color (loop [st (list 0)
                     vis #{0}
                     color (assoc (vec (repeat n -1)) (nth (nth tris 0) 0) 0 (nth (nth tris 0) 1) 1 (nth (nth tris 0) 2) 2)]
                (if (empty? st) color
                  (let [tt (peek st)
                        [q2 vis2 color2] (loop [e 0 q (pop st) vis vis color color]
                                           (if (>= e 3) [q vis color]
                                             (let [u (nth (nth tris tt) e) v (nth (nth tris tt) (mod (inc e) 3))
                                                   k (if (< u v) [u v] [v u])
                                                   [q3 vis3 color3] (reduce (fn [[q vis color] s]
                                                                             (if (contains? vis s) [q vis color]
                                                                               (let [w (first (remove #{u v} (nth tris s)))]
                                                                                 [(conj q s) (conj vis s) (assoc color w (- 3 (nth color u) (nth color v)))])))
                                                                           [q vis color] (get emap k []))]
                                               (recur (inc e) q3 vis3 color3))))]
                    (recur q2 vis2 color2))))
        cls (mapv (fn [c] (vec (sort (keep-indexed (fn [i col] (when (= col c) i)) color)))) (range 3))
        best (reduce (fn [b c] (let [a (nth cls c) bb (nth cls b)]
                                 (if (or (< (count a) (count bb))
                                         (and (= (count a) (count bb)) (neg? (compare a bb)))) c b))) 0 (range 1 3))
        guards (vec (sort (map #(nth w2o %) (nth cls best))))]
    (println (count guards))
    (println (apply str (interpose " " guards)))))
(-main)
