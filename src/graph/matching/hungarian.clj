(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          n (Integer/parseInt (nth toks 0))
          m (Integer/parseInt (nth toks 1))
          stride (inc m)
          ^longs cost (long-array (* (inc n) stride))
          ^longs u (long-array (inc n))
          ^longs v (long-array (inc m))
          ^ints p (int-array (inc m))
          ^ints way (int-array (inc m))
          inf 1000000000000000000]

      (loop [i 1, ti 2]
        (when (<= i n)
          (loop [j 1, tj ti]
            (when (<= j m)
              (aset cost (+ (* i stride) j) (Long/parseLong (nth toks tj)))
              (recur (inc j) (inc tj))))
          (recur (inc i) (+ ti m))))

      (loop [i 1]
        (when (<= i n)
          (aset p 0 i)
          (let [^longs minv (long-array (inc m) inf)
                used (boolean-array (inc m))]
            (loop [j0 0]
              (aset used j0 true)
              (let [i0 (aget p j0)
                    res (loop [j 1, delta inf, j1 0]
                          (if (<= j m)
                            (if (not (aget used j))
                              (let [cur (- (aget cost (+ (* i0 stride) j)) (aget u i0) (aget v j))]
                                (when (< cur (aget minv j))
                                  (aset minv j cur)
                                  (aset way j j0))
                                (if (< (aget minv j) delta)
                                  (recur (inc j) (aget minv j) j)
                                  (recur (inc j) delta j1)))
                              (recur (inc j) delta j1))
                            [delta j1]))
                    delta (nth res 0)
                    j1 (nth res 1)]
                (loop [j 0]
                  (when (<= j m)
                    (if (aget used j)
                      (do
                        (aset u (aget p j) (+ (aget u (aget p j)) delta))
                        (aset v j (- (aget v j) delta)))
                      (aset minv j (- (aget minv j) delta)))
                    (recur (inc j))))
                (if (not= (aget p j1) 0)
                  (recur j1)
                  (loop [jj j1]
                    (when (not= jj 0)
                      (let [pj (aget way jj)]
                        (aset p jj (aget p pj))
                        (recur pj))))))))
          (recur (inc i))))

      (println (- (aget v 0)))
      (let [^ints match (int-array (inc n))]
        (loop [j 1]
          (when (<= j m)
            (let [pj (aget p j)]
              (when (<= pj n)
                (aset match pj j)))
            (recur (inc j))))
        (loop [i 1]
          (when (<= i n)
            (println i (aget match i))
            (recur (inc i))))))))
