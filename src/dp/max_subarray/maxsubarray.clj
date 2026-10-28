(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (.split (.trim ^String s) "\\s+")
          t (Integer/parseInt (aget ^objects toks 0))
          out (StringBuilder.)]
      (loop [tc (int 0) p (int 1)]
        (when (< tc t)
          (let [n (Integer/parseInt (aget ^objects toks p))
                first-val (Long/parseLong (aget ^objects toks (inc p)))]
            (let [res (loop [i (int 1)
                             cur-sub first-val
                             max-sub first-val
                             max-elem first-val
                             pos-sum (if (> first-val 0) first-val 0)]
                        (if (< i n)
                          (let [v (Long/parseLong (aget ^objects toks (+ p 1 i)))
                                ncur (max v (+ cur-sub v))
                                nsub (max max-sub ncur)
                                nelem (max max-elem v)
                                npos (if (> v 0) (+ pos-sum v) pos-sum)]
                            (recur (inc i) ncur nsub nelem npos))
                          (let [subseq (if (> pos-sum 0) pos-sum max-elem)]
                            [max-sub subseq])))]
              (.append out (nth res 0))
              (.append out \space)
              (.append out (nth res 1))
              (.append out \newline))
            (recur (inc tc) (+ p 1 n)))))
      (print (str out)))))
