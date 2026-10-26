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
            (let [s (aget ^longs nums p)
                  n (aget ^longs nums (inc p))
                  dp (int-array (inc s))]
              (loop [k (long 0) q (+ p 2)]
                (when (< k n)
                  (let [w (aget ^longs nums q)
                        v (aget ^longs nums (inc q))]
                    (loop [c s]
                      (when (>= c w)
                        (let [nv (+ (aget ^ints dp (- c w)) v)]
                          (when (> nv (aget ^ints dp c))
                            (aset ^ints dp c (int nv))))
                        (recur (dec c)))))
                  (recur (inc k) (+ q 2))))
              (.append sb (aget ^ints dp s))
              (.append sb "\n")
              (recur (inc tc) (+ p 2 (* 2 n))))))
        (print (str sb))))))
