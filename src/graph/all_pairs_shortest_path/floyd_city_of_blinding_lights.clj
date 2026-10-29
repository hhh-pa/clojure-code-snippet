(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (.split (.trim ^String s) "\\s+")
          n (Integer/parseInt (aget ^objects toks 0))
          m (Integer/parseInt (aget ^objects toks 1))
          inf (int 1000000000)
          dist (make-array Integer/TYPE (inc n) (inc n))]
      (dotimes [i (inc n)]
        (dotimes [j (inc n)]
          (aset-int dist i j (if (= i j) 0 inf))))
      (dotimes [i m]
        (let [u (Integer/parseInt (aget ^objects toks (+ 2 (* i 3))))
              v (Integer/parseInt (aget ^objects toks (+ 3 (* i 3))))
              w (Integer/parseInt (aget ^objects toks (+ 4 (* i 3))))]
          (aset-int dist u v w)))
      (loop [k (int 1)]
        (when (<= k n)
          (loop [i (int 1)]
            (when (<= i n)
              (let [dik (aget dist i k)]
                (when (< dik inf)
                  (loop [j (int 1)]
                    (when (<= j n)
                      (let [dkj (aget dist k j)]
                        (when (< dkj inf)
                          (let [sum (+ dik dkj)]
                            (when (< sum (aget dist i j))
                              (aset-int dist i j sum)))))
                      (recur (inc j))))))
              (recur (inc i))))
          (recur (inc k))))
      (let [pos (+ 2 (* m 3))
            q (Integer/parseInt (aget ^objects toks pos))
            out (StringBuilder.)]
        (dotimes [i q]
          (let [u (Integer/parseInt (aget ^objects toks (+ pos 1 (* i 2))))
                v (Integer/parseInt (aget ^objects toks (+ pos 2 (* i 2))))
                d (aget dist u v)]
            (.append out (if (>= d inf) -1 d))
            (.append out \newline)))
        (print (str out))))))
