(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (.split (.trim ^String s) "\\s+")
          n (Integer/parseInt (aget ^objects toks 0))
          tails (int-array n)]
      (let [len (loop [i (int 0) cur-len (int 0)]
                  (if (< i n)
                    (let [x (Integer/parseInt (aget ^objects toks (inc i)))
                          idx (loop [l (int 0) r (int cur-len)]
                                (if (< l r)
                                  (let [mid (quot (+ l r) 2)]
                                    (if (< (aget tails mid) x)
                                      (recur (inc mid) r)
                                      (recur l mid)))
                                  l))]
                      (aset-int tails idx x)
                      (recur (inc i) (if (= idx cur-len) (inc cur-len) cur-len)))
                    cur-len))]
        (println len)))))
