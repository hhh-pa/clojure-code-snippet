(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (.split (.trim ^String s) "\\s+")
          n-toks (alength ^objects toks)
          out (StringBuilder.)]
      (loop [idx (int 0)]
        (when (< idx n-toks)
          (let [t (Integer/parseInt (aget ^objects toks idx))]
            (loop [tc (int 0) p (int (inc idx))]
              (when (< tc t)
                (let [n (Integer/parseInt (aget ^objects toks p))
                      m (Integer/parseInt (aget ^objects toks (inc p)))
                      adj (object-array (inc n))]
                  (dotimes [i (inc n)]
                    (aset ^objects adj i (java.util.ArrayList.)))
                  (dotimes [i m]
                    (let [u (Integer/parseInt (aget ^objects toks (+ p 2 (* i 2))))
                          v (Integer/parseInt (aget ^objects toks (+ p 3 (* i 2))))]
                      (.add ^java.util.ArrayList (aget ^objects adj u) (int v))
                      (.add ^java.util.ArrayList (aget ^objects adj v) (int u))))
                  (let [src (Integer/parseInt (aget ^objects toks (+ p 2 (* m 2))))
                        dist (int-array (inc n) -1)
                        q (java.util.ArrayDeque.)]
                    (aset-int dist src 0)
                    (.add ^java.util.ArrayDeque q src)
                    (loop []
                      (when-not (.isEmpty ^java.util.ArrayDeque q)
                        (let [u (int (.poll ^java.util.ArrayDeque q))
                              d (aget dist u)
                              neighbors ^java.util.ArrayList (aget ^objects adj u)
                              sz (.size neighbors)]
                          (dotimes [i sz]
                            (let [v (int (.get neighbors i))]
                              (when (= (aget dist v) -1)
                                (aset-int dist v (+ d 6))
                                (.add ^java.util.ArrayDeque q v))))
                          (recur))))
                    (let [first? (boolean-array 1 true)]
                      (loop [i (int 1)]
                        (when (<= i n)
                          (when (not= i src)
                            (if (aget first? 0)
                              (do (.append out (aget dist i)) (aset first? 0 false))
                              (do (.append out \space) (.append out (aget dist i)))))
                          (recur (inc i)))))
                    (.append out \newline)
                    (recur (inc tc) (+ p 3 (* m 2))))))))))
      (print (str out)))))
