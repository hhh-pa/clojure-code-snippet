(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (.split (.trim ^String s) "\\s+")
          n (Integer/parseInt (aget ^objects toks 0))
          m (Integer/parseInt (aget ^objects toks 1))
          adj (object-array (inc n))]
      (dotimes [i (inc n)]
        (aset ^objects adj i (java.util.ArrayList.)))
      (dotimes [i m]
        (let [u (Integer/parseInt (aget ^objects toks (+ 2 (* i 3))))
              v (Integer/parseInt (aget ^objects toks (+ 3 (* i 3))))
              w (Integer/parseInt (aget ^objects toks (+ 4 (* i 3))))]
          (.add ^java.util.ArrayList (aget ^objects adj u) (int-array [v w]))
          (.add ^java.util.ArrayList (aget ^objects adj v) (int-array [u w]))))
      (let [start (Integer/parseInt (aget ^objects toks (+ 2 (* m 3))))
            visited (boolean-array (inc n))
            pq (java.util.PriorityQueue. 1024
                 (reify java.util.Comparator
                   (compare [_ a b]
                     (Integer/compare (aget ^ints a 0) (aget ^ints b 0)))))]
        (.add pq (int-array [0 start]))
        (let [tot-mst (loop [count-visited (int 0) total-weight (long 0)]
                        (if (or (= count-visited n) (.isEmpty pq))
                          total-weight
                          (let [top ^ints (.poll pq)
                                w (aget top 0)
                                u (aget top 1)]
                            (if (aget visited u)
                              (recur count-visited total-weight)
                              (do
                                (aset visited u true)
                                (let [neighbors ^java.util.ArrayList (aget ^objects adj u)
                                      sz (.size neighbors)]
                                  (dotimes [i sz]
                                    (let [edge ^ints (.get neighbors i)
                                          v (aget edge 0)
                                          ew (aget edge 1)]
                                      (when-not (aget visited v)
                                        (.add pq (int-array [ew v]))))))
                                (recur (inc count-visited) (+ total-weight w)))))))]
          (println tot-mst))))))
