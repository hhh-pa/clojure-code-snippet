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
      (when (>= nd 2)
        (let [vv (aget d 0) ee (aget d 1)
              m (int (min (long ee) (long (quot (- nd 2) 2))))
              g (make-array java.util.ArrayList vv)
              indeg (int-array vv)]
          (dotimes [i vv] (aset g i (java.util.ArrayList.)))
          (dotimes [k m]
            (let [from (aget d (+ 2 (* k 2)))
                  to (aget d (+ 3 (* k 2)))]
              (.add ^java.util.ArrayList (aget g from) (Integer. to))
              (aset-int indeg to (unchecked-inc-int (aget indeg to)))))
          ;; Kahn with a max-heap: every pop takes the current maximum, so the
          ;; output order is fully determined (identical to the reference's
          ;; std::priority_queue<int> run).
          (let [pq (java.util.PriorityQueue. (int (max 1 vv))
                                             (java.util.Collections/reverseOrder))
                order (int-array vv)
                ans (int-array vv)]
            (loop [i (unchecked-dec-int vv)]
              (when (>= i 0)
                (when (zero? (aget ^ints indeg i))
                  (.add ^java.util.PriorityQueue pq (Integer. i)))
                (recur (unchecked-dec-int i))))
            (loop [counter (int 0)]
              (if (.isEmpty ^java.util.PriorityQueue pq)
                (do
                  (dotimes [i vv] (aset-int ans (aget ^ints order i) i))
                  (let [sb (StringBuilder.)]
                    (dotimes [i vv]
                      (.append sb (int (aget ^ints ans i)))
                      (.append sb \space))
                    (.append sb \newline)
                    (print (.toString sb))))
                (let [v (int (.poll ^java.util.PriorityQueue pq))]
                  (aset-int order v counter)
                  (let [nbrs ^java.util.ArrayList (aget g v)]
                    (loop [j (int 0)]
                      (when (< j (.size nbrs))
                        (let [w (int (.get nbrs j))
                              ni (unchecked-dec-int (aget ^ints indeg w))]
                          (aset-int indeg w ni)
                          (when (zero? ni)
                            (.add ^java.util.PriorityQueue pq (Integer. w))))
                        (recur (unchecked-inc-int j)))))
                  (recur (unchecked-inc-int counter)))))))))))
