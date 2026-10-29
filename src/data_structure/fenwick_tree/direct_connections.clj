(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (.split (.trim ^String s) "\\s+")
          t (Integer/parseInt (aget ^objects toks 0))
          mod (long 1000000007)
          out (StringBuilder.)]
      (loop [tc (int 0) p (int 1)]
        (if (>= tc t)
          (print (str out))
          (let [n (Integer/parseInt (aget ^objects toks p))
                x-coords (long-array n)
                pop (long-array n)]
            (dotimes [i n]
              (aset-long x-coords i (Long/parseLong (aget ^objects toks (+ p 1 i)))))
            (dotimes [i n]
              (aset-long pop i (Long/parseLong (aget ^objects toks (+ p 1 n i)))))
            (let [sorted-x (long-array (java.util.Arrays/copyOf x-coords n))]
              (java.util.Arrays/sort sorted-x)
              (let [indices (int-array n)]
                (dotimes [i n] (aset-int indices i i))
                (let [sorted-indices (vec (sort-by (fn [^long i] (aget pop (int i))) (seq indices)))
                      tree-count (long-array (inc n))
                      tree-sum (long-array (inc n))
                      update (fn [^longs bit ^long idx ^long val]
                               (loop [i (int idx)]
                                 (when (<= i n)
                                   (aset-long bit i (rem (+ (aget bit i) val) mod))
                                   (recur (unchecked-add-int i (bit-and i (- i)))))))
                      query (fn [^longs bit ^long idx]
                              (loop [i (int idx) sum (long 0)]
                                (if (> i 0)
                                  (recur (unchecked-subtract-int i (bit-and i (- i)))
                                         (rem (+ sum (aget bit i)) mod))
                                  sum)))]
                  (loop [step (int 0) ans (long 0)]
                    (if (< step n)
                      (let [idx (int (nth sorted-indices step))
                            xi (aget x-coords idx)
                            pi (rem (aget pop idx) mod)
                            rank (inc (java.util.Arrays/binarySearch sorted-x xi))
                            cnt-l (query tree-count rank)
                            sum-l (query tree-sum rank)
                            tot-cnt (query tree-count n)
                            tot-sum (query tree-sum n)
                            cnt-r (rem (+ (- tot-cnt cnt-l) mod) mod)
                            sum-r (rem (+ (- tot-sum sum-l) mod) mod)
                            left-contrib (rem (+ (- (* cnt-l (rem xi mod)) sum-l) mod) mod)
                            right-contrib (rem (+ (- sum-r (* cnt-r (rem xi mod))) mod) mod)
                            step-ans (rem (* pi (rem (+ left-contrib right-contrib) mod)) mod)]
                        (update tree-count rank 1)
                        (update tree-sum rank xi)
                        (recur (inc step) (rem (+ ans step-ans) mod)))
                      (do
                        (.append out ans)
                        (.append out \newline)))))))
            (recur (inc tc) (+ p 1 (* 2 n)))))))))
