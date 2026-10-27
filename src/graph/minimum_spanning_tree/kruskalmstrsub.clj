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
              ^ints us (int-array m) ^ints vs (int-array m) ^ints ws (int-array m)]
          (dotimes [k m]
            (let [b3 (unchecked-multiply-int (int k) (int 3))]
              (aset-int us k (unchecked-subtract-int (aget d (unchecked-add-int b3 (int 2))) (int 1)))
              (aset-int vs k (unchecked-subtract-int (aget d (unchecked-add-int b3 (int 3))) (int 1)))
              (aset-int ws k (aget d (unchecked-add-int b3 (int 4))))))
          (let [^longs packed (long-array m)
                ^ints parent (int-array g-nodes) ^ints sz (int-array g-nodes)]
            ;; Edge order by ascending weight: pack (weight, index)
            ;; into longs, sort with java.util.Arrays/sort (dual-pivot,
            ;; robust on sorted/uniform input, deterministic; equal
            ;; weights keep index order). No boxed seq or key calls.
            (dotimes [k m]
              (aset-long packed k (bit-or (bit-shift-left (long (aget ws k)) 32)
                                          (long k))))
            (java.util.Arrays/sort packed)
            (dotimes [i g-nodes] (aset-int parent i i) (aset-int sz i 1))
            (letfn [(find [x]
                      (loop [x (int x)]
                        (let [p (aget parent x)]
                          (if (= p x)
                            x
                            (recur p)))))]
              (loop [ki (int 0) acc (long 0)]
                (if (>= ki m)
                  (print (str acc "\n"))
                  (let [k (unchecked-int (aget packed ki))
                        x (aget us k) y (aget vs k)
                        rx (find x) ry (find y)]
                    (if (= rx ry)
                      (recur (unchecked-inc-int ki) acc)
                      (let [sx (aget sz rx) sy (aget sz ry)]
                        ;; union by size; path halving on the two find paths
                        (loop [z (int x)]
                          (let [p (aget parent z)]
                            (when (not= p rx)
                              (aset-int parent z rx)
                              (recur p))))
                        (loop [z (int y)]
                          (let [p (aget parent z)]
                            (when (not= p ry)
                              (aset-int parent z ry)
                              (recur p))))
                        (if (>= sx sy)
                          (do (aset-int parent ry rx)
                              (aset-int sz rx (+ sx sy)))
                          (do (aset-int parent rx ry)
                              (aset-int sz ry (+ sx sy))))
                        (recur (unchecked-inc-int ki) (unchecked-add acc (long (aget ws k))))))))))))))))
