;; center-star: 2-approximation for multiple sequence alignment.
;; The center minimizing total pairwise edit distance is aligned to
;; every other string; pairwise alignments merge with "once a gap,
;; always a gap". Pairwise backtrack prefers diagonal, then up, then
;; left. Output: sum-of-pairs score, then aligned rows in input order.
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
(defn edit-dp [a b]
  (let [n (count a) m (count b) w (inc m)
        idx (fn [i j] (+ (* i w) j))]
    [(reduce (fn [dp i]
               (reduce (fn [dp j]
                         (assoc dp (idx i j)
                                (min (+ (nth dp (idx (dec i) j)) 1)
                                     (+ (nth dp (idx i (dec j))) 1)
                                     (+ (nth dp (idx (dec i) (dec j)))
                                        (if (= (nth a (dec i)) (nth b (dec j))) 0 1)))))
                       dp (range 1 (inc m))))
             (vec (for [i (range (inc n)) j (range (inc m))] (cond (zero? j) i (zero? i) j :else 0)))
             (range 1 (inc n))) idx n m]))
(defn edist [a b]
  (let [[dp idx n m] (edit-dp a b)] (nth dp (idx n m))))
(defn align2 [c s]
  (let [n (count c) m (count s)
        [dp idx _ _] (edit-dp c s)
        [rcv rsv] (loop [i n j m rc [] rs []]
                    (if (and (zero? i) (zero? j)) [rc rs]
                      (let [cur (nth dp (idx i j))]
                        (cond
                          (and (pos? i) (pos? j)
                               (= cur (+ (nth dp (idx (dec i) (dec j)))
                                         (if (= (nth c (dec i)) (nth s (dec j))) 0 1))))
                          (recur (dec i) (dec j) (conj rc (nth c (dec i))) (conj rs (nth s (dec j))))
                          (and (pos? i) (= cur (+ (nth dp (idx (dec i) j)) 1)))
                          (recur (dec i) j (conj rc (nth c (dec i))) (conj rs \-))
                          :else
                          (recur i (dec j) (conj rc \-) (conj rs (nth s (dec j))))))))]
    [(apply str (reverse rcv)) (apply str (reverse rsv))]))
(defn merge-rows [rows nstr sstr]
  (let [r (nth rows 0) nr (count rows)]
    (loop [i 0 j 0 acc (vec (repeat (inc nr) ""))]
      (if (and (>= i (count r)) (>= j (count nstr))) acc
        (cond
          (and (< j (count nstr)) (= (nth nstr j) \-))
          (recur i (inc j) (assoc (vec (mapv #(str % "-") (subvec acc 0 nr))) nr (str (nth acc nr) (nth sstr j))))
          (and (< i (count r)) (= (nth r i) \-))
          (recur (inc i) j (assoc (vec (mapv (fn [k] (str (nth acc k) (nth (nth rows k) i))) (range nr))) nr (str (nth acc nr) "-")))
          :else
          (recur (inc i) (inc j) (assoc (vec (mapv (fn [k] (str (nth acc k) (nth (nth rows k) i))) (range nr))) nr (str (nth acc nr) (nth sstr j)))))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))
        n (parse-token (nth toks 0))
        strs (vec (take n (drop 1 toks)))
        totals (mapv (fn [c] (reduce + (map #(edist (nth strs c) %) strs))) (range n))
        best (loop [c 1 b 0 bt (nth totals 0)]
               (if (>= c n) b (if (< (nth totals c) bt) (recur (inc c) c (nth totals c)) (recur (inc c) b bt))))
        [rows rowpos] (loop [idx 0 rows [(nth strs best)] rowpos (assoc (vec (repeat n 0)) best 0)]
                        (if (>= idx n) [rows rowpos]
                          (if (= idx best) (recur (inc idx) rows rowpos)
                            (let [[ns ss] (align2 (nth strs best) (nth strs idx))
                                  nr (merge-rows rows ns ss)]
                              (recur (inc idx) nr (assoc rowpos idx (count rows)))))))
        sp (reduce + (for [col (range (count (nth rows 0))) a (range (count rows)) b (range (inc a) (count rows))]
                       (if (= (nth (nth rows a) col) (nth (nth rows b) col)) 0 1)))]
    (println sp)
    (loop [i 0] (when (< i n) (println (nth rows (nth rowpos i))) (recur (inc i))))))
(-main)
