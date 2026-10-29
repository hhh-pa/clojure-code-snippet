(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          v-count (Integer/parseInt (nth toks 0))
          e-count (Integer/parseInt (nth toks 1))
          ^ints edge-u (int-array e-count)
          ^ints edge-v (int-array e-count)
          ^longs edge-w (long-array e-count)
          adj (make-array Object v-count)
          _ (loop [i 0] (when (< i v-count) (aset adj i (java.util.ArrayList.)) (recur (inc i))))]
      (loop [i 0, ti 2]
        (when (< i e-count)
          (let [u (Integer/parseInt (nth toks ti))
                v (Integer/parseInt (nth toks (+ ti 1)))
                w (Long/parseLong (nth toks (+ ti 2)))]
            (aset edge-u i u)
            (aset edge-v i v)
            (aset edge-w i w)
            (.add ^java.util.ArrayList (aget adj u) [v w]))
          (recur (inc i) (+ ti 3))))

      (let [^longs h (long-array v-count)
            neg-cycle?
            (loop [iter 0]
              (if (< iter v-count)
                (let [updated? (loop [j 0, upd false]
                                 (if (< j e-count)
                                   (let [u (aget edge-u j)
                                         v (aget edge-v j)
                                         w (aget edge-w j)
                                         cand (+ (aget h u) w)]
                                     (if (< cand (aget h v))
                                       (do
                                         (aset h v cand)
                                         (recur (inc j) true))
                                       (recur (inc j) upd)))
                                   upd))]
                  (if-not updated?
                    false
                    (if (== iter (dec v-count))
                      true
                      (recur (inc iter)))))
                false))]

        (if neg-cycle?
          (println "NEGATIVE CYCLE")
          (let [inf 1000000000000000000
                sb (StringBuilder.)]
            (loop [src 0]
              (when (< src v-count)
                (let [^longs d (long-array v-count inf)]
                  (aset d src 0)
                  (let [pq (java.util.PriorityQueue.
                             (reify java.util.Comparator
                               (compare [_ a b]
                                 (Long/compare (first a) (first b)))))]
                    (.add pq [0 src])
                    (while (not (.isEmpty pq))
                      (let [top (.poll pq)
                            cur-d (long (first top))
                            u (int (second top))]
                        (when (<= cur-d (aget d u))
                          (let [^java.util.ArrayList neighbors (aget adj u)
                                nsz (.size neighbors)]
                            (loop [idx 0]
                              (when (< idx nsz)
                                (let [edge (.get neighbors idx)
                                      v (int (first edge))
                                      w (long (second edge))
                                      reweighted-w (+ w (- (aget h u) (aget h v)))
                                      cand (+ (aget d u) reweighted-w)]
                                  (when (< cand (aget d v))
                                    (aset d v cand)
                                    (.add pq [cand v])))
                                (recur (inc idx)))))))))

                  (loop [v 0]
                    (when (< v v-count)
                      (when (> v 0) (.append sb " "))
                      (let [dist (aget d v)]
                        (if (>= dist (/ inf 2))
                          (.append sb "INF")
                          (.append sb (str (+ dist (- (aget h v) (aget h src)))))))
                      (recur (inc v))))
                  (.append sb "\n"))
                (recur (inc src))))
            (print (.toString sb))))))))
