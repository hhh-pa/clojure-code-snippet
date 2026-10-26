(require '[clojure.string :as str])
(let [data (slurp *in*)]
  (when-not (str/blank? data)
    (let [toks (.split (.trim ^String data) "\\s+")
          nums (long-array (alength ^objects toks))]
      (dotimes [i (alength ^objects toks)]
        (aset ^longs nums i (Long/parseLong (aget ^objects toks i))))
      ;; Mirror the C++ exactly: x = b (len xm), y = a (len ym).
      (let [n (aget ^longs nums 0)
            m (aget ^longs nums 1)
            a (int-array n)
            b (int-array m)]
        (dotimes [i n]
          (aset ^ints a i (int (aget ^longs nums (+ 2 i)))))
        (dotimes [i m]
          (aset ^ints b i (int (aget ^longs nums (+ 2 n i)))))
        (let [xm m
              ym n
              stride (inc ym)
              L (int-array (* (inc xm) stride))]
          (dotimes [i (inc xm)]
            (dotimes [j (inc ym)]
              (let [idx (+ (* i stride) j)]
                (cond
                  (or (zero? i) (zero? j))
                  (aset ^ints L idx (int 0))
                  (== (aget ^ints b (dec i)) (aget ^ints a (dec j)))
                  (aset ^ints L idx (unchecked-inc-int (aget ^ints L (+ (* (dec i) stride) (dec j)))))
                  :else
                  (let [u (aget ^ints L (+ (* (dec i) stride) j))
                        v (aget ^ints L (+ (* i stride) (dec j)))]
                    (aset ^ints L idx (if (> u v) u v)))))))
          (let [ans (loop [i xm j ym acc (transient [])]
                      (if (or (zero? i) (zero? j))
                        (persistent! acc)
                        (if (== (aget ^ints b (dec i)) (aget ^ints a (dec j)))
                          (recur (dec i) (dec j) (conj! acc (aget ^ints b (dec i))))
                          (if (> (aget ^ints L (+ (* (dec i) stride) j))
                                 (aget ^ints L (+ (* i stride) (dec j))))
                            (recur (dec i) j acc)
                            (recur i (dec j) acc)))))]
            (let [sb (StringBuilder.)]
              (doseq [x (reverse ans)]
                (.append sb (str x))
                (.append sb " "))
              (.append sb "\n")
              (print (str sb)))))))))
