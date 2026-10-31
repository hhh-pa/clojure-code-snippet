;; dcel-overlay: planar overlay of two simple polygons in general
;; position. Exact integer overlay vertices (endpoints plus proper
;; crossings), half-edges with twins, CCW edge order (integer angle
;; sort at vertices, orientation rule at crossings), next-pointer
;; face traversal. Output: V E F with F = cycles-C+1.
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
(defn proper? [a b c d]
  (let [d1 (cross c d a) d2 (cross c d b) d3 (cross a b c) d4 (cross a b d)]
    (and (or (and (pos? d1) (neg? d2)) (and (neg? d1) (pos? d2)))
         (or (and (pos? d3) (neg? d4)) (and (neg? d3) (pos? d4))))))
(defn tparam [a b c d]
  (let [rx (- (nth b 0) (nth a 0)) ry (- (nth b 1) (nth a 1))
        qx (- (nth d 0) (nth c 0)) qy (- (nth d 1) (nth c 1))
        num (- (* (- (nth c 0) (nth a 0)) qy) (* (- (nth c 1) (nth a 1)) qx))
        den (- (* rx qy) (* ry qx))]
    (if (neg? den) [(- num) (- den)] [num den])))
(defn index-of [c x]
  (first (keep-indexed (fn [i v] (when (= v x) i)) c)))
(defn -main []
  (let [toks (tokenize (slurp *in*)) nums (mapv parse-token toks)
        n1 (nth nums 0)
        p1 (vec (for [i (range n1)] [(nth nums (+ 1 (* 2 i))) (nth nums (+ 2 (* 2 i)))]))
        n2 (nth nums (+ 1 (* 2 n1)))
        base (+ 2 (* 2 n1))
        p2 (vec (for [j (range n2)] [(nth nums (+ base (* 2 j))) (nth nums (+ base (inc (* 2 j))))]))
        xs (vec (for [i (range n1) j (range n2)
                      :let [a (nth p1 i) b (nth p1 (mod (inc i) n1)) c (nth p2 j) d (nth p2 (mod (inc j) n2))]
                      :when (proper? a b c d)]
                  [i j (tparam a b c d) (tparam c d a b)]))
        X (count xs)
        V (+ n1 n2 X)
        on1 (mapv (fn [i] (vec (sort-by (fn [k] (let [[nn dd] (nth (nth xs k) 2)] (/ nn dd)))
                                        (filter #(= (nth (nth xs %) 0) i) (range X))))) (range n1))
        on2 (mapv (fn [j] (vec (sort-by (fn [k] (let [[nn dd] (nth (nth xs k) 3)] (/ nn dd)))
                                        (filter #(= (nth (nth xs %) 1) j) (range X))))) (range n2))
        ues (vec (concat
                  (mapcat (fn [i] (let [ch (vec (concat [i] (map #(+ n1 n2 %) (nth on1 i)) [(mod (inc i) n1)]))]
                                            (map (fn [a b] [a b 1 i]) ch (rest ch)))) (range n1))
                  (mapcat (fn [j] (let [ch (vec (concat [(+ n1 j)] (map #(+ n1 n2 %) (nth on2 j)) [(+ n1 (mod (inc j) n2))]))]
                                            (map (fn [a b] [a b 2 j]) ch (rest ch)))) (range n2))))
        E (count ues)
        hes (vec (mapcat (fn [k [u v poly e]] [[u v (inc (* 2 k)) poly e] [v u (* 2 k) poly e]])
                         (range E) ues))
        H (count hes)
        out (mapv (fn [v] (filterv #(= (nth (nth hes %) 0) v) (range H))) (range V))
        dir-of (fn [v h]
                 (let [[_ _ _ poly e] (nth hes h)]
                   (if (= poly 1)
                     (let [a (nth p1 e) b (nth p1 (mod (inc e) n1)) vv (nth p1 v)
                           w (if (= vv a) b a)]
                       [(- (nth w 0) (nth vv 0)) (- (nth w 1) (nth vv 1))])
                     (let [a (nth p2 e) b (nth p2 (mod (inc e) n2)) vv (nth p2 (- v n1))
                           w (if (= vv a) b a)]
                       [(- (nth w 0) (nth vv 0)) (- (nth w 1) (nth vv 1))]))))
        ccw-orig (mapv (fn [v]
                         (vec (sort (fn [h1 h2]
                                      (let [d1 (dir-of v h1) d2 (dir-of v h2)
                                            q1 (if (or (pos? (nth d1 1)) (and (zero? (nth d1 1)) (pos? (nth d1 0)))) 0 1)
                                            q2 (if (or (pos? (nth d2 1)) (and (zero? (nth d2 1)) (pos? (nth d2 0)))) 0 1)]
                                        (if (not= q1 q2) (< q1 q2)
                                          (pos? (- (* (nth d1 0) (nth d2 1)) (* (nth d1 1) (nth d2 0)))))))
                                    (nth out v))))
                       (range (+ n1 n2)))
        ccw-x (mapv (fn [k]
                      (let [v (+ n1 n2 k) e1 (nth (nth xs k) 0) e2 (nth (nth xs k) 1)
                            a (nth p1 e1) b (nth p1 (mod (inc e1) n1))
                            c (nth p2 e2)
                            ch1 (vec (concat [e1] (map #(+ n1 n2 %) (nth on1 e1)) [(mod (inc e1) n1)]))
                            ch2 (vec (concat [(+ n1 e2)] (map #(+ n1 n2 %) (nth on2 e2)) [(+ n1 (mod (inc e2) n2))]))
                            i1 (index-of ch1 v) i2 (index-of ch2 v)
                            toA (nth ch1 (dec i1)) toB (nth ch1 (inc i1))
                            toC (nth ch2 (dec i2))
                            by-to (into {} (map (fn [h] [(nth (nth hes h) 1) h]) (nth out v)))
                            hA (get by-to toA) hB (get by-to toB) hC (get by-to toC)
                            hD (first (remove #{hA hB hC} (nth out v)))]
                        (if (pos? (cross a b c)) [hB hC hA hD] [hB hD hA hC])))
                    (range X))
        ccw (vec (concat ccw-orig ccw-x))
        pos (reduce (fn [p v] (reduce (fn [p [i h]] (assoc p h i)) p (map-indexed vector (nth ccw v))))
                    (vec (repeat H -1)) (range V))
        nxt (mapv (fn [h] (let [w (nth (nth hes h) 1) tw (nth (nth hes h) 2) cyc (nth ccw w)]
                             (nth cyc (mod (dec (nth pos tw)) (count cyc))))) (range H))
        cycles (loop [h 0 vis #{} cyc 0]
                 (if (>= h H) cyc
                   (if (contains? vis h) (recur (inc h) vis cyc)
                     (recur (inc h) (loop [c h vis vis] (if (contains? vis c) vis (recur (nth nxt c) (conj vis c)))) (inc cyc)))))
        adj (reduce (fn [adj [u v _ _]] (-> adj (update u (fnil conj #{}) v) (update v (fnil conj #{}) u)))
                    (vec (repeat V #{})) ues)
        comps (loop [v 0 vis #{} c 0]
                (if (>= v V) c
                  (if (contains? vis v) (recur (inc v) vis c)
                    (recur (inc v) (loop [st [v] vis vis]
                                     (if (empty? st) vis
                                       (let [u (peek st)]
                                         (if (contains? vis u) (recur (pop st) vis)
                                           (recur (into (pop st) (remove vis (nth adj u))) (conj vis u)))))) (inc c)))))]
    (println V E (- (+ cycles 1) comps))))
(-main)
