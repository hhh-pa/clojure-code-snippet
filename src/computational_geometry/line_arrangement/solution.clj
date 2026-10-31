;; line-arrangement: incremental arrangement of lines ax+by=c.
;; Each new line meets previous lines in its zone; vertices print as
;; reduced fractions sorted by value, then E = n+2V and F = n+V+1
;; (sphere compactification; inputs have no three concurrent lines).
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
(defn gg [a b]
  (loop [a (if (neg? a) (- a) a) b (if (neg? b) (- b) b)]
    (if (zero? b) a (recur b (mod a b)))))
(defn -main []
  (let [nums (mapv parse-token (tokenize (slurp *in*)))
        n (nth nums 0)
        abc (vec (for [i (range n)] [(nth nums (+ 1 (* 3 i))) (nth nums (+ 2 (* 3 i))) (nth nums (+ 3 (* 3 i)))]))
        verts (vec (for [i (range n) j (range (inc i) n)
                         :let [[a1 b1 c1] (nth abc i) [a2 b2 c2] (nth abc j)
                               det (- (* a1 b2) (* a2 b1))]
                         :when (not (zero? det))]
                     (let [xn (- (* c1 b2) (* c2 b1)) yn (- (* a1 c2) (* a2 c1))
                           gx (gg xn det) gy (gg yn det)
                           xd (if (neg? det) (- det) det)
                           sxn (if (neg? det) (- xn) xn) syn (if (neg? det) (- yn) yn)]
                       [(quot sxn gx) (quot xd gx) (quot syn gy) (quot xd gy)])))
        sverts (vec (sort (fn [[axn axd ayn ayd] [bxn bxd byn byd]]
                            (let [l (* axn bxd) r (* bxn axd)]
                              (if (not= l r) (< l r) (< (* ayn byd) (* byn ayd))))) verts))
        V (count sverts)]
    (println V)
    (doseq [[xn xd yn yd] sverts] (println (str xn "/" xd " " yn "/" yd)))
    (println (str (+ n (* 2 V)) " " (+ n V 1)))))
(-main)
