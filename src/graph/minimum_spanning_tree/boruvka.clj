(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          n (Integer/parseInt (nth toks 0))
          m (Integer/parseInt (nth toks 1))
          ^ints eu (int-array m)
          ^ints ev (int-array m)
          ^longs ew (long-array m)
          ^ints parent (int-array n)
          ^ints rank (int-array n)
          ^ints cheapest (int-array n)
          ncomp (int-array 1)]

      (letfn [(find-root [i]
                (let [p (aget parent i)]
                  (if (== p i)
                    i
                    (let [r (find-root p)]
                      (aset parent i r)
                      r))))

              (unite [i j]
                (let [ri (find-root i)
                      rj (find-root j)]
                  (if (== ri rj)
                    false
                    (let [rri (aget rank ri)
                          rrj (aget rank rj)
                          a (if (< rri rrj) rj ri)
                          b (if (< rri rrj) ri rj)]
                      (aset parent b a)
                      (when (== (aget rank a) (aget rank b))
                        (aset rank a (inc (aget rank a))))
                      (aset ncomp 0 (dec (aget ncomp 0)))
                      true))))]

        (loop [i 0]
          (when (< i n)
            (aset parent i i)
            (recur (inc i))))
        (aset ncomp 0 n)

        (loop [i 0, ti 2]
          (when (< i m)
            (aset eu i (Integer/parseInt (nth toks ti)))
            (aset ev i (Integer/parseInt (nth toks (+ ti 1))))
            (aset ew i (Long/parseLong (nth toks (+ ti 2))))
            (recur (inc i) (+ ti 3))))

        (loop [mst-weight 0, edges-added 0]
          (if (> (aget ncomp 0) 1)
            (do
              (java.util.Arrays/fill cheapest -1)
              (loop [i 0]
                (when (< i m)
                  (let [ru (find-root (aget eu i))
                        rv (find-root (aget ev i))]
                    (when (not= ru rv)
                      (let [cu (aget cheapest ru)]
                        (when (or (== cu -1) (< (aget ew i) (aget ew cu)))
                          (aset cheapest ru i)))
                      (let [cv (aget cheapest rv)]
                        (when (or (== cv -1) (< (aget ew i) (aget ew cv)))
                          (aset cheapest rv i)))))
                  (recur (inc i))))
              (let [[new-weight new-added progress]
                    (loop [i 0, w mst-weight, c edges-added, p false]
                      (if (< i n)
                        (let [e (aget cheapest i)]
                          (if (not= e -1)
                            (if (unite (aget eu e) (aget ev e))
                              (recur (inc i) (+ w (aget ew e)) (inc c) true)
                              (recur (inc i) w c p))
                            (recur (inc i) w c p)))
                        [w c p]))]
                (if progress
                  (recur new-weight new-added)
                  (if (== new-added (dec n))
                    (println new-weight)
                    (println "DISCONNECTED")))))
            (if (== edges-added (dec n))
              (println mst-weight)
              (println "DISCONNECTED"))))))))
