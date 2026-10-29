(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          n (Integer/parseInt (nth toks 0))
          m (Integer/parseInt (nth toks 1))
          s (Integer/parseInt (nth toks 2))
          t (Integer/parseInt (nth toks 3))
          max-e (* 2 (max 1 m))
          ^ints edge-to (int-array max-e)
          ^ints edge-rev (int-array max-e)
          ^longs edge-cap (long-array max-e)
          ^longs edge-flow (long-array max-e)
          ^ints edge-next (int-array max-e -1)
          ^ints head (int-array n -1)
          ^ints level (int-array n)
          ^ints ptr (int-array n)
          ^ints q (int-array (max 10 n))
          edge-cnt (int-array 1)]

      (letfn [(add-edge [from to-node c]
                (let [e1 (aget edge-cnt 0)
                      e2 (inc e1)]
                  (aset edge-cnt 0 (+ e1 2))
                  (aset edge-to e1 to-node)
                  (aset edge-rev e1 e2)
                  (aset edge-cap e1 c)
                  (aset edge-flow e1 0)
                  (aset edge-next e1 (aget head from))
                  (aset head from e1)

                  (aset edge-to e2 from)
                  (aset edge-rev e2 e1)
                  (aset edge-cap e2 0)
                  (aset edge-flow e2 0)
                  (aset edge-next e2 (aget head to-node))
                  (aset head to-node e2)))

              (bfs []
                (java.util.Arrays/fill level -1)
                (aset level s 0)
                (aset q 0 s)
                (loop [qh 0, qt 1]
                  (if (< qh qt)
                    (let [v (aget q qh)
                          new-qt (loop [e (aget head v), cur-qt qt]
                                   (if (not= e -1)
                                     (let [tr (aget edge-to e)
                                           rem-cap (- (aget edge-cap e) (aget edge-flow e))]
                                       (if (and (> rem-cap 0) (== (aget level tr) -1))
                                         (do
                                           (aset level tr (inc (aget level v)))
                                           (aset q cur-qt tr)
                                           (recur (aget edge-next e) (inc cur-qt)))
                                         (recur (aget edge-next e) cur-qt)))
                                     cur-qt))]
                      (recur (inc qh) new-qt))
                    nil))
                (not= (aget level t) -1))

              (dfs [v pushed]
                (cond
                  (== pushed 0) 0
                  (== v t) pushed
                  :else
                  (loop []
                    (let [e (aget ptr v)]
                      (if (== e -1)
                        0
                        (let [tr (aget edge-to e)
                              rem-cap (- (aget edge-cap e) (aget edge-flow e))]
                          (if (and (== (aget level tr) (inc (aget level v))) (> rem-cap 0))
                            (let [tr-pushed (dfs tr (min pushed rem-cap))]
                              (if (> tr-pushed 0)
                                (do
                                  (aset edge-flow e (+ (aget edge-flow e) tr-pushed))
                                  (let [r (aget edge-rev e)]
                                    (aset edge-flow r (- (aget edge-flow r) tr-pushed)))
                                  tr-pushed)
                                (do
                                  (aset ptr v (aget edge-next e))
                                  (recur))))
                            (do
                              (aset ptr v (aget edge-next e))
                              (recur)))))))))]

        (loop [i 0, ti 4]
          (when (< i m)
            (let [u (Integer/parseInt (nth toks ti))
                  v (Integer/parseInt (nth toks (+ ti 1)))
                  cap (Long/parseLong (nth toks (+ ti 2)))]
              (add-edge u v cap)
              (recur (inc i) (+ ti 3)))))

        (let [inf 1000000000000000000]
          (loop [flow 0]
            (if (bfs)
              (do
                (System/arraycopy head 0 ptr 0 n)
                (let [pushed-flow (loop [round-flow 0]
                                    (let [pushed (dfs s inf)]
                                      (if (> pushed 0)
                                        (recur (+ round-flow pushed))
                                        round-flow)))]
                  (recur (+ flow pushed-flow))))
              (println flow))))))))
