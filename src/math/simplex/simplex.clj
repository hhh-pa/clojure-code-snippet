(java.util.Locale/setDefault java.util.Locale/US)
(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          m (Integer/parseInt (nth toks 0))
          n (Integer/parseInt (nth toks 1))
          rows (inc m)
          cols (+ n m 1)
          rhs-col (+ n m)
          eps 1e-9
          inf 1e18
          ^doubles tab (double-array (* rows cols))]
      (let [ti-after-c (loop [j 0, ti 2]
                         (if (< j n)
                           (do
                             (aset tab (+ (* m cols) j) (- (Double/parseDouble (nth toks ti))))
                             (recur (inc j) (inc ti)))
                           ti))]
        (loop [i 0, ti ti-after-c]
          (when (< i m)
            (loop [j 0]
              (when (< j n)
                (aset tab (+ (* i cols) j) (Double/parseDouble (nth toks (+ ti j))))
                (recur (inc j))))
            (aset tab (+ (* i cols) (+ n i)) 1.0)
            (aset tab (+ (* i cols) rhs-col) (Double/parseDouble (nth toks (+ ti n))))
            (recur (inc i) (+ ti n 1))))
        (loop []
          (let [enter-col (loop [j 0, best -1, best-val (- eps)]
                            (if (< j (+ n m))
                              (let [v (aget tab (+ (* m cols) j))]
                                (if (< v best-val)
                                  (recur (inc j) j v)
                                  (recur (inc j) best best-val)))
                              best))]
            (if (== enter-col -1)
              (do
                (println "OPTIMAL")
                (println (format "%.2f" (aget tab (+ (* m cols) rhs-col)))))
              (let [leave-row (loop [i 0, best -1, best-ratio inf]
                                (if (< i m)
                                  (let [coeff (aget tab (+ (* i cols) enter-col))]
                                    (if (> coeff eps)
                                      (let [ratio (/ (aget tab (+ (* i cols) rhs-col)) coeff)]
                                        (if (< ratio (- best-ratio eps))
                                          (recur (inc i) i ratio)
                                          (recur (inc i) best best-ratio)))
                                      (recur (inc i) best best-ratio)))
                                  best))]
                (if (== leave-row -1)
                  (println "UNBOUNDED")
                  (do
                    (let [pivot (aget tab (+ (* leave-row cols) enter-col))]
                      (loop [j 0]
                        (when (<= j rhs-col)
                          (let [k (+ (* leave-row cols) j)]
                            (aset tab k (/ (aget tab k) pivot)))
                          (recur (inc j))))
                      (loop [i 0]
                        (when (<= i m)
                          (when (not= i leave-row)
                            (let [factor (aget tab (+ (* i cols) enter-col))]
                              (when (> (Math/abs factor) eps)
                                (loop [j 0]
                                  (when (<= j rhs-col)
                                    (let [k (+ (* i cols) j)]
                                      (aset tab k (- (aget tab k) (* factor (aget tab (+ (* leave-row cols) j))))))
                                    (recur (inc j)))))))
                          (recur (inc i))))
                    (recur))))))))))))
