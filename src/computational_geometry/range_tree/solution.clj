;; range-tree: 2D orthogonal range reporting with fractional cascading.
;; Primary x-tree nodes store subtree points by y plus cascade
;; pointers into children's y-arrays; one binary search at the split
;; node, constant-time range propagation down both query paths. Each
;; query prints its match count and the sorted points as x,y pairs.
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
(def BIG 4000000000000000000)
(def BIGM 3999999999999999999)
(defn build-node [tree pts xs l r]
  (let [yv (vec (sort-by (fn [i] [(nth (nth pts i) 1) (nth (nth pts i) 0)]) (subvec xs l r)))]
    (if (= (- r l) 1)
      [(conj tree {:l l :r r :mid l :left -1 :right -1 :yvec yv :fcl [] :fcr []}) (count tree)]
      (let [mid (quot (+ l r) 2)
            [t1 L] (build-node tree pts xs l mid)
            [t2 R] (build-node t1 pts xs mid r)
            ly (:yvec (nth t2 L)) ry (:yvec (nth t2 R))
            cas (fn [cy] (mapv (fn [i] (let [yb (if (< i (count yv)) (nth (nth pts (nth yv i)) 1) BIG)]
                                          (count (take-while #( < (nth (nth pts %) 1) yb) cy)))) (range (inc (count yv)))))
            node {:l l :r r :mid mid :left L :right R :yvec yv :fcl (cas ly) :fcr (cas ry)}]
        [(conj t2 node) (count t2)]))))
(defn query-one [tree pts xs root x1 y1 x2 y2]
  (let [v (loop [v root]
            (let [nd (nth tree v)]
              (if (= (:left nd) -1) v
                (let [key (nth (nth pts (nth xs (:mid nd))) 0)]
                  (cond (< x2 key) (recur (:left nd))
                        (> x1 key) (recur (:right nd))
                        :else v)))))
        nd (nth tree v)]
    (if (= (:left nd) -1)
      (let [pid (nth (:yvec nd) 0) p (nth pts pid)]
        (if (and (<= x1 (nth p 0) x2) (<= y1 (nth p 1) y2)) [pid] []))
      (let [syv (:yvec nd)
            lo0 (count (take-while #( < (nth (nth pts %) 1) y1) syv))
            y2p (if (= y2 BIGM) BIG (inc y2))
            hi0 (count (take-while #( < (nth (nth pts %) 1) y2p) syv))
            lv (:left nd) rv (:right nd)
            walk (fn [u a b is-left]
                   (loop [u u a a b b acc []]
                     (let [nd (nth tree u)]
                       (if (= (:left nd) -1)
                         (if (< a b)
                           (let [pid (nth (:yvec nd) 0) px (nth (nth pts pid) 0)]
                             (if (if is-left (<= x1 px) (<= px x2)) (conj acc pid) acc))
                           acc)
                         (let [key (nth (nth pts (nth xs (:mid nd))) 0)]
                           (if (if is-left (<= x1 key) (<= key x2))
                             (let [c (if is-left (:right nd) (:left nd))
                                   f (if is-left (:fcr nd) (:fcl nd))
                                   g (if is-left (:fcl nd) (:fcr nd))]
                               (recur (if is-left (:left nd) (:right nd))
                                      (nth g a) (nth g b)
                                      (into acc (subvec (:yvec (nth tree c)) (nth f a) (nth f b)))))
                             (recur (if is-left (:right nd) (:left nd))
                                    (nth (if is-left (:fcr nd) (:fcl nd)) a)
                                    (nth (if is-left (:fcr nd) (:fcl nd)) b) acc)))))))]
        (concat (walk lv (nth (:fcl nd) lo0) (nth (:fcl nd) hi0) true)
                (walk rv (nth (:fcr nd) lo0) (nth (:fcr nd) hi0) false))))))
(defn -main []
  (let [nums (mapv parse-token (tokenize (slurp *in*)))
        n (nth nums 0)
        pts (vec (for [i (range n)] [(nth nums (+ 1 (* 2 i))) (nth nums (+ 2 (* 2 i)))]))
        xs (vec (sort-by (fn [i] [(nth (nth pts i) 0) (nth (nth pts i) 1)]) (range n)))
        [tree root] (build-node [] pts xs 0 n)
        base (+ 1 (* 2 n))
        q (nth nums base)
        qs (vec (for [i (range q)] [(nth nums (+ base 1 (* 4 i))) (nth nums (+ base 2 (* 4 i)))
                                           (nth nums (+ base 3 (* 4 i))) (nth nums (+ base 4 (* 4 i)))]))]
    (doseq [[x1 y1 x2 y2] qs]
      (let [found (vec (sort-by (fn [i] [(nth (nth pts i) 0) (nth (nth pts i) 1)])
                                (query-one tree pts xs root x1 y1 x2 y2)))]
        (println (count found))
        (when (seq found)
          (println (apply str (interpose " " (map (fn [i] (str (nth (nth pts i) 0) "," (nth (nth pts i) 1))) found)))))))))
(-main)
