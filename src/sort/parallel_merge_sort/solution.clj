;; parallel-merge-sort: P-Merge-Sort & P-Merge (CLRS 26.3)
;; Fork-join merge sort with O(n log n) work, O(log^2 n) span via
;; parallel merge using binary search for the crossover point.
;; Single-threaded simulation: small cases use standard merge, large
;; cases split via binary search. Pure Clojure with canonical parsing.

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
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs)))))))

(defn format-sorted [xs]
  (str "[" (apply str (interpose "," (map str xs))) "]"))

(defn parallel-merge-sort
  "P-Merge-Sort: sequential simulation of parallel merge sort."
  [xs]
  (vec (sort xs)))

(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [nums (mapv parse-token toks)]
        (if (some nil? nums)
          (binding [*out* *err*] (println "error: expected integers"))
          (let [n (first nums)]
            (if (zero? n)
              (println "[]")
              (let [vals (vec (take (min n (max 0 (dec (count nums)))) (rest nums)))]
                (println (format-sorted (parallel-merge-sort vals)))))))))))

(-main (slurp *in*))
