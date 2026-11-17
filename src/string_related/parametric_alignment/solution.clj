;; 1D parametric alignment: exact decomposition over gap penalty.
(require '[clojure.string :as str])

(let [toks (vec (.split (.trim (slurp *in*)) "\\s+"))
      match (Long/parseLong (nth toks 1)) mismatch (Long/parseLong (nth toks 2))
      g0 (Long/parseLong (nth toks 3)) g1 (Long/parseLong (nth toks 4))
      s1 (nth toks 5) s2 (nth toks 6)
      n (count s1) m (count s2) k (+ n m)
      neg -1000000000000000000
      dp (make-array Long/TYPE (inc n) (inc m) (inc k))
      par (make-array Byte/TYPE (inc n) (inc m) (inc k))]
  (doseq [i (range (inc n)) j (range (inc m)) t (range (inc k))]
    (aset-long dp i j t neg))
  (aset-long dp 0 0 0 0)
  (doseq [i (range 1 (inc n))] (aset-long dp i 0 i 0) (aset-byte par i 0 i 2))
  (doseq [j (range 1 (inc m))] (aset-long dp 0 j j 0) (aset-byte par 0 j j 3))
  (doseq [i (range 1 (inc n)) j (range 1 (inc m))]
    (let [sub (if (= (nth s1 (dec i)) (nth s2 (dec j))) match mismatch)]
      (doseq [t (range (inc k))]
        (let [dv (aget dp (dec i) (dec j) t)
              uv (if (pos? t) (aget dp (dec i) j (dec t)) neg)
              lv (if (pos? t) (aget dp i (dec j) (dec t)) neg)
              d (if (= dv neg) neg (+ dv sub))
              cands (filter #(not= (first %) neg) [[d 1] [uv 2] [lv 3]])
              best (if (empty? cands) [neg 0] (reduce (fn [a b] (if (> (first b) (first a)) b a)) cands))]
          (aset-long dp i j t (first best))
          (aset-byte par i j t (second best))))))
  (let [lines (vec (for [s (range (inc k))
                         :let [c (aget dp n m s)]
                         :when (not= c neg)]
                     (loop [i n j m t s mt 0 ms 0]
                       (if (and (zero? i) (zero? j)) [(- s) c mt ms s]
                           (case (aget par i j t)
                             1 (recur (dec i) (dec j) t
                                      (if (= (nth s1 (dec i)) (nth s2 (dec j))) (inc mt) mt)
                                      (if (= (nth s1 (dec i)) (nth s2 (dec j))) ms (inc ms)))
                             2 (recur (dec i) j (dec t) mt ms)
                             (recur i (dec j) (dec t) mt ms))))))
        lines (vec (distinct (sort-by #(nth % 4) > lines)))
        ix (fn [a b] (/ (- (nth b 1) (nth a 1)) (- (nth a 0) (nth b 0))))
        hull (reduce (fn [h ln]
                        (loop [h h]
                          (if (and (>= (count h) 2) (>= (ix (nth h (- (count h) 2)) (peek h)) (ix (peek h) ln)))
                            (recur (pop h))
                            (conj h ln))))
                      [] lines)
        segs (vec (for [i (range (count hull))
                        :let [ln (nth hull i)
                              lo (if (zero? i) g0 (ix (nth hull (dec i)) ln))
                              hi (if (= i (dec (count hull))) g1 (ix ln (nth hull (inc i))))
                              lo (max lo g0) hi (min hi g1)]
                        :when (< lo hi)]
                    [lo hi (nth ln 2) (nth ln 3) (nth ln 4)]))
        segs (if (empty? segs)
               (let [best (apply max-key (fn [ln] (+ (nth ln 1) (* (nth ln 0) g0))) hull)]
                 [[g0 g1 (nth best 2) (nth best 3) (nth best 4)]])
               segs)]
    (print (str/join "\n" (map (fn [[lo hi mt ms s]] (str lo " " hi " " mt " " ms " " s)) segs)))))
