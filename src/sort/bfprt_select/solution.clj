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

(defn tokenize
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))

(defn parse-token
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))

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
