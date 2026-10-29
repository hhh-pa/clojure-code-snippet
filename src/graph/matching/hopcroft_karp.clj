(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          n (Integer/parseInt (nth toks 0))
          m (Integer/parseInt (nth toks 1))
          e (Integer/parseInt (nth toks 2))
          max-e (max 1 e)
          ^ints edge-to (int-array max-e)
          ^ints edge-next (int-array max-e -1)
          ^ints head (int-array (+ n 1) -1)
          ^ints tail (int-array (+ n 1) -1)
          ^ints pair-u (int-array (+ n 1))
          ^ints pair-v (int-array (+ m 1))
          ^ints dist (int-array (+ n 1))
          ^ints q (int-array (+ n 1))
          edge-cnt (int-array 1)
          inf 1000000000]

      (letfn [(add-edge [u v]
                (let [e1 (aget edge-cnt 0)]
                  (aset edge-cnt 0 (inc e1))
                  (aset edge-to e1 v)
                  (aset edge-next e1 -1)
                  (if (== (aget head u) -1)
                    (do
                      (aset head u e1)
                      (aset tail u e1))
                    (do
                      (aset edge-next (aget tail u) e1)
                      (aset tail u e1)))))

              (bfs []
                (let [qt (loop [u 1, qt 0]
                           (if (<= u n)
                             (if (== (aget pair-u u) 0)
                               (do
                                 (aset dist u 0)
                                 (aset q qt u)
                                 (recur (inc u) (inc qt)))
                               (do
                                 (aset dist u inf)
                                 (recur (inc u) qt)))
                             qt))]
                  (aset dist 0 inf)
                  (loop [qh 0, qt qt]
                    (if (< qh qt)
                      (let [u (aget q qh)
                            new-qt (if (< (aget dist u) (aget dist 0))
                                     (loop [e (aget head u), cur-qt qt]
                                       (if (not= e -1)
                                         (let [v (aget edge-to e)
                                               next-u (aget pair-v v)]
                                           (if (== (aget dist next-u) inf)
                                             (do
                                               (aset dist next-u (inc (aget dist u)))
                                               (aset q cur-qt next-u)
                                               (recur (aget edge-next e) (inc cur-qt)))
                                             (recur (aget edge-next e) cur-qt)))
                                         cur-qt))
                                     qt)]
                        (recur (inc qh) new-qt))
                      nil)))
                (not= (aget dist 0) inf))

              (dfs [u]
                (if (not= u 0)
                  (loop [e (aget head u)]
                    (if (== e -1)
                      (do
                        (aset dist u inf)
                        false)
                      (let [v (aget edge-to e)
                            next-u (aget pair-v v)]
                        (if (and (== (aget dist next-u) (inc (aget dist u)))
                                 (dfs next-u))
                          (do
                            (aset pair-v v u)
                            (aset pair-u u v)
                            true)
                          (recur (aget edge-next e))))))
                  true))]

        (loop [i 0, ti 3]
          (when (< i e)
            (let [u (Integer/parseInt (nth toks ti))
                  v (Integer/parseInt (nth toks (+ ti 1)))]
              (add-edge u v)
              (recur (inc i) (+ ti 2)))))

        (let [size (loop [matching 0]
                     (if (bfs)
                       (let [new-matching (loop [u 1, cnt matching]
                                            (if (<= u n)
                                              (recur (inc u)
                                                     (if (and (== (aget pair-u u) 0)
                                                              (dfs u))
                                                       (inc cnt)
                                                       cnt))
                                              cnt))]
                         (recur new-matching))
                       matching))]
          (println size)
          (loop [u 1]
            (when (<= u n)
              (when (not= (aget pair-u u) 0)
                (println u (aget pair-u u)))
              (recur (inc u)))))))))
