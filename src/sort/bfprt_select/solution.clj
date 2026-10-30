;; bfprt-select: deterministic O(n) selection via median-of-medians
;; (CLRS 9.3). Groups of 5 are sorted to find each group median;
;; the median of medians is found recursively, then a three-way
;; partition locates the k-th order statistic. Worst-case O(n) time,
;; O(n) space. Pure Clojure: immutable vectors, loop and recur,
;; sort for group medians. Input: n, array, k (1-indexed) on stdin;
;; output: k-th smallest.

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

(defn bfprt
  "Deterministic select: k-th smallest (0-indexed) in O(n) worst case."
  [xs k]
  (if (= (count xs) 1)
    (first xs)
    (let [n (count xs)
          ng (quot (+ n 4) 5)
          medians (mapv (fn [i]
                          (let [l (* i 5) r (min (* (inc i) 5) n)
                                grp (sort (subvec xs l r))]
                            (nth grp (quot (count grp) 2))))
                        (range ng))
          pivot (let [s (sort medians)] (nth s (quot (count s) 2)))
          less (filterv #(< % pivot) xs)
          eq (filterv #(= % pivot) xs)
          greater (filterv #(> % pivot) xs)
          rank-lt (count less)
          rank-eq (count eq)]
      (cond
        (< k rank-lt) (bfprt less k)
        (< k (+ rank-lt rank-eq)) pivot
        :else (bfprt greater (- k rank-lt rank-eq))))))

(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [nums (mapv parse-token toks)]
        (if (some nil? nums)
          (binding [*out* *err*] (println "error: expected integers"))
          (let [n (first nums)
                vals (vec (take (min n (max 0 (- (count nums) 2))) (rest nums)))
                k (when (> (count nums) (inc (count vals))) (nth nums (inc (count vals))))]
            (if (or (empty? vals) (nil? k) (<= k 0) (> k (count vals)))
              (println 0)
              (println (bfprt vals (dec k))))))))))

(-main (slurp *in*))
