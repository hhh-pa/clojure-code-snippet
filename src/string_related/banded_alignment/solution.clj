;; banded_alignment: Banded alignment: k-difference global alignment in O(kn) banded DP.
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
        s1 (nth toks 0 nil) s2 (nth toks 1 nil) k (when (nth toks 2 nil) (Integer/parseInt (nth toks 2)))]
    (when (and s1 s2 k)
      (let [n (count s1) m (count s2) INF 1000000000
            dp (vec (for [_ (range (inc n))] (vec (repeat (inc m) INF))))
            dp (assoc-in dp [0 0] 0)
            dp (reduce (fn [dp [i j]]
                     (if (> (Math/abs (- i j)) k) dp
                       (if (and (= i 0) (= j 0)) dp
                         (let [cands (cond-> []
                                       (and (> i 0) (<= (Math/abs (- (dec i) j)) k)) (conj (+ (get-in dp [(dec i) j]) 1))
                                       (and (> j 0) (<= (Math/abs (- i (dec j))) k)) (conj (+ (get-in dp [i (dec j)]) 1))
                                       (and (> i 0) (> j 0) (<= (Math/abs (- (dec i) (dec j))) k)) (conj (+ (get-in dp [(dec i) (dec j)]) (if (= (nth s1 (dec i)) (nth s2 (dec j))) 0 1))))
                               best (if (seq cands) (apply min cands) INF)]
                           (if (< best INF) (assoc-in dp [i j] best) dp)))))
                   dp (for [i (range (inc n)) j (range (inc m))] [i j]))]
        (let [ans (get-in dp [n m])] (println (if (>= ans INF) -1 ans)))))))
(-main)

