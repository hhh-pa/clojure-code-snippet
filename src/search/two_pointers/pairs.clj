(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (.split (.trim ^String s) "\\s+")
          n (Integer/parseInt (aget ^objects toks 0))
          k (Long/parseLong (aget ^objects toks 1))
          arr (long-array n)]
      (dotimes [i n]
        (aset-long arr i (Long/parseLong (aget ^objects toks (+ 2 i)))))
      (java.util.Arrays/sort arr)
      (let [ans (loop [i (int 0) j (int 1) count (long 0)]
                  (if (or (>= i n) (>= j n))
                    count
                    (if (<= j i)
                      (recur i (inc i) count)
                      (let [diff (- (aget arr j) (aget arr i))]
                        (cond
                          (= diff k) (recur (inc i) (inc j) (inc count))
                          (< diff k) (recur i (inc j) count)
                          :else (recur (inc i) j count))))))]
        (println ans)))))
