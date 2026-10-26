(require '[clojure.string :as str])
(let [data (slurp *in*)]
  (when-not (str/blank? data)
    (let [toks (.split (.trim ^String data) "\\s+")
          n (Long/parseLong (aget ^objects toks 0))
          m (Long/parseLong (aget ^objects toks 1))
          coins (long-array m)]
      (dotimes [i m]
        (aset ^longs coins i (Long/parseLong (aget ^objects toks (+ 2 i)))))
      (let [dp (long-array (inc n))]
        (aset ^longs dp 0 1)
        (dotimes [i m]
          (let [c (aget ^longs coins i)]
            (loop [j c]
              (when (<= j n)
                (aset ^longs dp j (+ (aget ^longs dp j) (aget ^longs dp (- j c))))
                (recur (inc j))))))
        (print (aget ^longs dp n))))))
