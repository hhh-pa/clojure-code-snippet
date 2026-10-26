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
          ^ints d data
          nd (alength d)]
      (when (>= nd 1)
        (let [t (aget d 0)
              out (StringBuilder.)]
          (loop [tc (int 0) pos (int 1)]
            (when (< tc t)
              (let [nn (aget d pos) mm (aget d (unchecked-inc-int pos))
                    base (unchecked-add-int pos (int 2))
                    deg (int-array nn)]
                ;; degree count (undirected => +1 each side)
                (dotimes [k mm]
                  (let [x (unchecked-dec-int (aget d (unchecked-add-int base (unchecked-multiply-int k (int 3)))))
                        y (unchecked-dec-int (aget d (unchecked-add-int base (unchecked-inc-int (unchecked-multiply-int k (int 3))))))]
                    (aset-int deg x (unchecked-inc-int (aget deg x)))
                    (aset-int deg y (unchecked-inc-int (aget deg y)))))
                ;; CSR offsets
                (let [off (int-array (unchecked-inc-int nn))]
                  (dotimes [i nn]
                    (aset-int off (unchecked-inc-int i)
                              (unchecked-add-int (aget off i) (aget deg i))))
                  (let [tot (aget off nn)
                        tos (int-array tot) wts (int-array tot)
                        cur (java.util.Arrays/copyOf ^ints off (unchecked-inc-int nn))]
                    (dotimes [k mm]
                      (let [x (unchecked-dec-int (aget d (unchecked-add-int base (unchecked-multiply-int k (int 3)))))
                            y (unchecked-dec-int (aget d (unchecked-add-int base (unchecked-inc-int (unchecked-multiply-int k (int 3))))))
                            r (aget d (unchecked-add-int base (+ 2 (unchecked-multiply-int k (int 3)))))]
                        ;; NOTE: re-read cur[y] after writing x's side so that
                        ;; self-loops (x == y) consume two slots, not one
                        (let [px (aget ^ints cur x)]
                          (aset-int tos px y) (aset-int wts px r)
                          (aset-int cur x (unchecked-inc-int px)))
                        (let [py (aget ^ints cur y)]
                          (aset-int tos py x) (aset-int wts py r)
                          (aset-int cur y (unchecked-inc-int py)))))
                    ;; dijkstra with stale-entry skipping (same distances as the
                    ;; reference's set-based decrease-key version)
                    (let [src (unchecked-dec-int (aget d (unchecked-add-int base (unchecked-multiply-int mm (int 3)))))
                          dist (int-array nn)]
                      (java.util.Arrays/fill ^ints dist (int -1))
                      (aset-int dist src 0)
                      (let [pq (java.util.PriorityQueue.)]
                        (.add ^java.util.PriorityQueue pq (long src))
                        (loop []
                          (when-not (.isEmpty ^java.util.PriorityQueue pq)
                            (let [top (long (.poll ^java.util.PriorityQueue pq))
                                  cd (int (bit-shift-right top 32))
                                  v (int (bit-and top 4294967295))]
                              (when (= cd (aget ^ints dist v))
                                (let [e0 (aget ^ints off v) e1 (aget ^ints off (unchecked-inc-int v))]
                                  (loop [e (int e0)]
                                    (when (< e e1)
                                      (let [to (aget ^ints tos e)
                                            ndv (unchecked-add-int cd (aget ^ints wts e))
                                            old (aget ^ints dist to)]
                                        (when (or (= old -1) (< ndv old))
                                          (aset-int dist to ndv)
                                          (.add ^java.util.PriorityQueue pq
                                                (bit-or (bit-shift-left (long ndv) 32) (long to)))))
                                      (recur (unchecked-inc-int e)))))))
                            (recur))))
                      (dotimes [i nn]
                        (when (not= i src)
                          (.append ^StringBuilder out (int (aget ^ints dist i)))
                          (.append ^StringBuilder out \space)))
                      (.append ^StringBuilder out \newline))))
                (recur (unchecked-inc-int tc)
                       (unchecked-add-int base (unchecked-add-int (unchecked-multiply-int mm (int 3)) (int 1)))))))
          (print (.toString ^StringBuilder out)))))))
