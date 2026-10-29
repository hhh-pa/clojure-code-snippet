(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          n (Integer/parseInt (nth toks 0))
          m (Integer/parseInt (nth toks 1))
          ^ints u-arr (int-array m)
          ^ints v-arr (int-array m)
          ^longs w-arr (long-array m)
          _ (loop [i 0, ti 2]
              (when (< i m)
                (aset u-arr i (Integer/parseInt (nth toks ti)))
                (aset v-arr i (Integer/parseInt (nth toks (+ ti 1))))
                (aset w-arr i (Long/parseLong (nth toks (+ ti 2))))
                (recur (inc i) (+ ti 3))))
          indices (int-array m)
          _ (loop [i 0] (when (< i m) (aset indices i i) (recur (inc i))))
          ;; Sort indices by w-arr
          boxed-indices (java.util.ArrayList. m)
          _ (loop [i 0] (when (< i m) (.add boxed-indices i) (recur (inc i))))
          _ (java.util.Collections/sort boxed-indices
              (reify java.util.Comparator
                (compare [_ a b]
                  (Long/compare (aget w-arr (int a)) (aget w-arr (int b))))))
          ^ints parent (int-array n)
          _ (loop [i 0] (when (< i n) (aset parent i i) (recur (inc i))))]

      (letfn [(find-root [i]
                (if (== (aget parent i) i)
                  i
                  (let [r (find-root (aget parent i))]
                    (aset parent i r)
                    r)))
              (unite [a b]
                (let [ra (find-root a)
                      rb (find-root b)]
                  (if (== ra rb)
                    false
                    (do
                      (aset parent rb ra)
                      true))))]

        (let [mst-edges (java.util.ArrayList.)
              weight-ref (long-array 1)
              count-ref (int-array 1)]
          (loop [idx 0]
            (when (< idx m)
              (let [e-idx (int (.get boxed-indices idx))
                    u (aget u-arr e-idx)
                    v (aget v-arr e-idx)
                    w (aget w-arr e-idx)]
                (when (unite u v)
                  (aset weight-ref 0 (+ (aget weight-ref 0) w))
                  (aset count-ref 0 (inc (aget count-ref 0)))
                  (let [umin (min u v)
                        umax (max u v)]
                    (.add mst-edges [umin umax])))
                (recur (inc idx)))))

          (java.util.Collections/sort mst-edges
            (reify java.util.Comparator
              (compare [_ a b]
                (let [u1 (int (first a))
                      u2 (int (first b))]
                  (if (not= u1 u2)
                    (Integer/compare u1 u2)
                    (Integer/compare (int (second a)) (int (second b))))))))

          (let [sb (StringBuilder.)]
            (.append sb (str "GHS MST Weight: " (aget weight-ref 0) "\n"))
            (.append sb (str "Edges: " (aget count-ref 0) "\n"))
            (let [sz (.size mst-edges)]
              (loop [i 0]
                (when (< i sz)
                  (let [pair (.get mst-edges i)]
                    (.append sb (str (first pair) " " (second pair) "\n")))
                  (recur (inc i)))))
            (print (.toString sb))))))))
