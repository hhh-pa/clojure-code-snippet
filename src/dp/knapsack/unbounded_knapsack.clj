(require '[clojure.string :as str])
(let [data (slurp *in*)]
  (when-not (str/blank? data)
    (let [toks (.split (.trim ^String data) "\\s+")
          nums (long-array (alength ^objects toks))]
      (dotimes [i (alength ^objects toks)]
        (aset ^longs nums i (Long/parseLong (aget ^objects toks i))))
      (let [t (aget ^longs nums 0)
            sb (StringBuilder.)]
        (loop [tc (long 0) p 1]
          (when (< tc t)
            (let [n (aget ^longs nums p)
                  s (aget ^longs nums (inc p))
                  vals (int-array n)
                  dp (int-array (inc s))]
              (dotimes [i n]
                (aset ^ints vals i (int (aget ^longs nums (+ p 2 i)))))
              (loop [c (long 0)]
                (when (<= c s)
                  (loop [k (long 0)]
                    (when (< k n)
                      (let [w (aget ^ints vals k)]
                        (when (<= w c)
                          (let [nv (+ (aget ^ints dp (- c w)) w)]
                            (when (> nv (aget ^ints dp c))
                              (aset ^ints dp c (int nv))))))
                      (recur (inc k))))
                  (recur (inc c))))
              (.append sb (aget ^ints dp s))
              (.append sb "\n")
              (recur (inc tc) (+ p 2 n)))))
        (print (str sb))))))
