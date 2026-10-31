;; segment-stabbing: report segments containing query points.
;; Elementary intervals from unique endpoints; each segment lives at
;; the canonical nodes covering its span; a point query collects the
;; sets along its root-leaf path. Matching ids print sorted.
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
(defn st-insert [t v l r ql qr id]
  (cond (or (>= ql r) (<= qr l)) t
        (and (<= ql l) (>= qr r)) (assoc t v (conj (nth t v) id))
        :else (let [m (quot (+ l r) 2)]
                (st-insert (st-insert t (* 2 v) l m ql qr id) (inc (* 2 v)) m r ql qr id))))
(defn st-query [t v l r i]
  (let [here (nth t v)]
    (if (= (- r l) 1) here
      (let [m (quot (+ l r) 2)]
        (concat here (if (< i m) (st-query t (* 2 v) l m i) (st-query t (inc (* 2 v)) m r i)))))))
(defn -main []
  (let [nums (mapv parse-token (tokenize (slurp *in*)))
        n (nth nums 0)
        segs (vec (for [i (range n)] [(nth nums (+ 1 (* 2 i))) (nth nums (+ 2 (* 2 i)))]))
        u (vec (sort (set (mapcat identity segs))))
        m (count u)
        N (inc (* 2 m))
        rank (zipmap u (range))
        t (reduce (fn [t [i [l r]]] (st-insert t 1 0 N (inc (* 2 (get rank l))) (+ 2 (* 2 (get rank r))) i))
                  (vec (repeat (* 4 N) [])) (map-indexed vector segs))
        base (+ 1 (* 2 n))
        q (nth nums base)]
    (loop [i 0]
      (when (< i q)
        (let [x (nth nums (+ base 1 i))
              ei (cond (< x (nth u 0)) 0
                       (> x (nth u (dec m))) (* 2 m)
                       :else (let [k (apply max (keep-indexed (fn [j v] (when (<= v x) j)) u))]
                               (if (= (nth u k) x) (inc (* 2 k)) (+ 2 (* 2 k)))))
              found (vec (sort (st-query t 1 0 N ei)))]
          (println (count found))
          (when (seq found) (println (apply str (interpose " " found)))))
        (recur (inc i))))))
(-main)
