(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (java.util.Scanner. ^String s)
          num-rules (.nextInt toks)
          start-symbol (int (.charAt (.next toks) 0))
          rules (vec (for [_ (range num-rules)
                           :let [lhs (int (.charAt (.next toks) 0))
                                 _ (.next toks)
                                 rhs (.next toks)]]
                       (if (= 1 (count rhs))
                         {:lhs lhs :term (int (.charAt ^String rhs 0))}
                         {:lhs lhs :rhs1 (int (.charAt ^String rhs 0)) :rhs2 (int (.charAt ^String rhs 1))})))
          num-queries (if (.hasNextInt toks) (.nextInt toks) 0)
          MOD (long 1000000007)]
      (dotimes [_ num-queries]
        (let [w (.next toks)
              n (count w)]
          (if (zero? n)
            (println "NO 0")
            (let [^longs dp (long-array (* (inc n) n 256))
                  idx (fn [len i c]
                        (+ (* (long len) (long n) 256) (* (long i) 256) (long c)))]
              (dotimes [i n]
                (let [ch (int (.charAt ^String w i))]
                  (doseq [r rules]
                    (when (= (:term r) ch)
                      (let [k (idx 1 i (:lhs r))]
                        (aset dp k (inc (aget dp k))))))))
              (loop [len (int 2)]
                (when (<= len n)
                  (dotimes [i (- (inc n) len)]
                    (dotimes [k (dec len)]
                      (let [left-len (inc k)
                            right-len (- len left-len)
                            left-start i
                            right-start (+ i left-len)]
                        (doseq [r rules]
                          (when-let [rhs1 (:rhs1 r)]
                            (let [rhs2 (:rhs2 r)
                                  left-ways (aget dp (idx left-len left-start rhs1))]
                              (when (pos? left-ways)
                                (let [right-ways (aget dp (idx right-len right-start rhs2))]
                                  (when (pos? right-ways)
                                    (let [add (rem (* (rem left-ways MOD) (rem right-ways MOD)) MOD)
                                          target (idx len i (:lhs r))]
                                      (aset dp target (rem (+ (aget dp target) add) MOD))))))))))))
                  (recur (inc len))))
              (let [ways (aget dp (idx n 0 start-symbol))]
                (if (pos? ways)
                  (println (str "YES " ways))
                  (println "NO 0"))))))))))
