;; gotoh_affine_gap: Gotoh affine gap: 3-matrix DP (M,Ix,Iy) for gap open/extend.
;; Pure Clojure with canonical input parsing.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

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
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs))))))))

(defn -main [input]
  (let [input (slurp *in*) toks (filter seq (str/split input #"\s+"))
        s1 (first toks) s2 (second toks)]
    (when (and s1 s2)
      (let [n (count s1) m (count s2) NEG -1000000000 MATCH 2 MISM -1 GO -2 GE -1
            M (vec (for [_ (range (inc n))] (vec (repeat (inc m) NEG))))
            Ix (vec (for [_ (range (inc n))] (vec (repeat (inc m) NEG))))
            Iy (vec (for [_ (range (inc n))] (vec (repeat (inc m) NEG))))
            M (assoc-in M [0 0] 0)
            Ix (reduce (fn [a i] (assoc-in a [i 0] (+ GO (* (dec i) GE)))) Ix (range 1 (inc n)))
            Iy (reduce (fn [a j] (assoc-in a [0 j] (+ GO (* (dec j) GE)))) Iy (range 1 (inc m)))
            [M Ix Iy] (reduce (fn [[M Ix Iy] [i j]]
                         (let [sc (if (= (nth s1 (dec i)) (nth s2 (dec j))) MATCH MISM)
                               best (max (get-in M [(dec i) (dec j)]) (get-in Ix [(dec i) (dec j)]) (get-in Iy [(dec i) (dec j)]))
                               nvM (+ best sc)
                               nvIx (max (+ (get-in M [(dec i) j]) GO) (+ (get-in Ix [(dec i) j]) GE))
                               nvIy (max (+ (get-in M [i (dec j)]) GO) (+ (get-in Iy [i (dec j)]) GE))]
                           [(assoc-in M [i j] nvM) (assoc-in Ix [i j] nvIx) (assoc-in Iy [i j] nvIy)]))
                       [M Ix Iy] (for [i (range 1 (inc n)) j (range 1 (inc m))] [i j]))]
        (println (max (get-in M [n m]) (get-in Ix [n m]) (get-in Iy [n m])))))))
(-main)

