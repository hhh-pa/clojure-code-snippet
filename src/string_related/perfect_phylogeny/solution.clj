;; perfect-phylogeny: directed (root all-0) perfect phylogeny test.
;; The matrix admits one iff the 1-sets of columns form a laminar
;; family: each pair nested or disjoint (Gusfield 17.1). A pair is
;; violated exactly when rows exhibit 11, 10 and 01 simultaneously.
;; Input: n m then n binary strings. Output: YES or NO.
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
(defn -main []
  (let [toks (tokenize (slurp *in*))
        n (parse-token (nth toks 0)) m (parse-token (nth toks 1))
        rows (vec (take n (drop 2 toks)))
        bad (first (for [j (range m) k (range (inc j) m)
                         :let [flags (reduce (fn [[h11 h10 h01] i]
                                               (let [a (nth (nth rows i) j) b (nth (nth rows i) k)]
                                                 (cond (and (= a \1) (= b \1)) [true h10 h01]
                                                       (= a \1) [h11 true h01]
                                                       (= b \1) [h11 h10 true]
                                                       :else [h11 h10 h01])))
                                             [false false false] (range n))]
                         :when (every? true? flags)] true))]
    (println (if bad "NO" "YES"))))
(-main)
