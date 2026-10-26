(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [n (int (count s))
          data (loop [i (int 0) c (int 0) ^ints a (int-array 1024)]
                 (if (>= i n)
                   (java.util.Arrays/copyOf a c)
                   (let [ch (int (.charAt ^String s i))]
                     (if (and (>= ch 48) (<= ch 57))
                       (let [r (loop [j (int i) v (int 0)]
                                 (if (< j n)
                                   (let [d (int (.charAt ^String s j))]
                                     (if (and (>= d 48) (<= d 57))
                                       (recur (unchecked-inc-int j)
                                              (unchecked-add-int
                                               (unchecked-multiply-int v (int 10))
                                               (unchecked-subtract-int d (int 48))))
                                       [j v]))
                                   [j v]))
                             j (int (nth r 0)) v (int (nth r 1))
                             a (if (>= c (alength a))
                                 (java.util.Arrays/copyOf a (unchecked-multiply-int (alength a) (int 2)))
                                 a)]
                         (aset-int a c v)
                         (recur j (unchecked-inc-int c) a))
                       (recur (unchecked-inc-int i) c a)))))
          nd (alength ^ints data)]
      (when (>= nd 2)
        (let [^ints d data
              g-nodes (aget d 0)
              g-edges (aget d 1)
              m (int (min (long g-edges) (long (quot (- nd 2) 3))))
              us (int-array m) vs (int-array m) ws (int-array m)]
          (dotimes [k m]
            (aset-int us k (- (aget d (+ 2 (* k 3))) 1))
            (aset-int vs k (- (aget d (+ 3 (* k 3))) 1))
            (aset-int ws k (aget d (+ 4 (* k 3)))))
          (let [order (sort-by #(aget ^ints ws %) (range m))
                parent (int-array g-nodes) sz (int-array g-nodes)]
            (dotimes [i g-nodes] (aset-int parent i i) (aset-int sz i 1))
            (letfn [(find [x]
                      (loop [x (int x)]
                        (let [p (aget ^ints parent x)]
                          (if (= p x)
                            x
                            (recur p)))))]
              (loop [os order acc (long 0)]
                (if (empty? os)
                  (print (str acc "\n"))
                  (let [k (int (first os))
                        x (aget ^ints us k) y (aget ^ints vs k)
                        rx (find x) ry (find y)]
                    (if (= rx ry)
                      (recur (rest os) acc)
                      (let [sx (aget ^ints sz rx) sy (aget ^ints sz ry)]
                        ;; union by size; path halving on the two find paths
                        (loop [z (int x)]
                          (let [p (aget ^ints parent z)]
                            (when (not= p rx)
                              (aset-int parent z rx)
                              (recur p))))
                        (loop [z (int y)]
                          (let [p (aget ^ints parent z)]
                            (when (not= p ry)
                              (aset-int parent z ry)
                              (recur p))))
                        (if (>= sx sy)
                          (do (aset-int parent ry rx)
                              (aset-int sz rx (+ sx sy)))
                          (do (aset-int parent rx ry)
                              (aset-int sz ry (+ sx sy))))
                        (recur (rest os) (+ acc (long (aget ^ints ws k))))))))))))))))
