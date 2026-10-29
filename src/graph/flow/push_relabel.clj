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
          adj (make-array Object n)
          _ (loop [i 0] (when (< i n) (aset adj i (java.util.ArrayList.)) (recur (inc i))))
          ^longs excess (long-array n)
          ^ints height (int-array n)
          ^ints count-h (int-array (* 2 n))
          ^booleans in-queue (boolean-array n)
          active-q (java.util.ArrayDeque.)
          edge-cnt (int-array 1)]

      (letfn [(add-edge [from to-node c]
                (let [e1 (aget edge-cnt 0)
                      e2 (inc e1)]
                  (aset edge-cnt 0 (+ e1 2))
                  (aset edge-to e1 to-node)
                  (aset edge-rev e1 e2)
                  (aset edge-cap e1 c)
                  (aset edge-flow e1 0)
                  (.add ^java.util.ArrayList (aget adj from) e1)

                  (aset edge-to e2 from)
                  (aset edge-rev e2 e1)
                  (aset edge-cap e2 0)
                  (aset edge-flow e2 0)
                  (.add ^java.util.ArrayList (aget adj to-node) e2)))

              (enqueue [u]
                (when (and (not (aget in-queue u))
                           (not= u s)
                           (not= u t)
                           (> (aget excess u) 0))
                  (aset in-queue u true)
                  (.add active-q u)))

              (push-flow [u e]
                (let [tr (aget edge-to e)
                      rem-cap (- (aget edge-cap e) (aget edge-flow e))
                      amt (min (aget excess u) rem-cap)]
                  (when (and (> amt 0) (== (aget height u) (inc (aget height tr))))
                    (aset edge-flow e (+ (aget edge-flow e) amt))
                    (let [r (aget edge-rev e)]
                      (aset edge-flow r (- (aget edge-flow r) amt)))
                    (aset excess u (- (aget excess u) amt))
                    (aset excess tr (+ (aget excess tr) amt))
                    (enqueue tr))))

              (relabel-node [u]
                (let [^java.util.ArrayList u-adj (aget adj u)
                      sz (.size u-adj)
                      min-h (loop [i 0, mh (* 2 n)]
                              (if (< i sz)
                                (let [e (int (.get u-adj i))
                                      rem-cap (- (aget edge-cap e) (aget edge-flow e))]
                                  (if (> rem-cap 0)
                                    (recur (inc i) (min mh (aget height (aget edge-to e))))
                                    (recur (inc i) mh)))
                                mh))]
                  (when (< min-h (* 2 n))
                    (let [old-h (aget height u)]
                      (aset count-h old-h (dec (aget count-h old-h)))
                      (let [new-h (inc min-h)]
                        (aset height u new-h)
                        (aset count-h new-h (inc (aget count-h new-h))))))))

              (discharge [u]
                (let [^java.util.ArrayList u-adj (aget adj u)]
                  (loop [idx 0]
                    (when (> (aget excess u) 0)
                      (if (< idx (.size u-adj))
                        (let [e (int (.get u-adj idx))
                              tr (aget edge-to e)
                              rem-cap (- (aget edge-cap e) (aget edge-flow e))]
                          (if (and (> rem-cap 0) (== (aget height u) (inc (aget height tr))))
                            (do
                              (push-flow u e)
                              (recur idx))
                            (recur (inc idx))))
                        (do
                          (relabel-node u)
                          (recur 0)))))))]

        (loop [i 0, ti 4]
          (when (< i m)
            (let [u (Integer/parseInt (nth toks ti))
                  v (Integer/parseInt (nth toks (+ ti 1)))
                  cap (Long/parseLong (nth toks (+ ti 2)))]
              (add-edge u v cap)
              (recur (inc i) (+ ti 3)))))

        (aset height s n)
        (aset count-h n 1)
        (aset count-h 0 (dec n))

        (let [^java.util.ArrayList s-adj (aget adj s)
              sz (.size s-adj)]
          (loop [i 0]
            (when (< i sz)
              (let [e (int (.get s-adj i))
                    amt (aget edge-cap e)]
                (when (> amt 0)
                  (let [tr (aget edge-to e)
                        r (aget edge-rev e)]
                    (aset edge-flow e (+ (aget edge-flow e) amt))
                    (aset edge-flow r (- (aget edge-flow r) amt))
                    (aset excess tr (+ (aget excess tr) amt))
                    (aset excess s (- (aget excess s) amt))
                    (enqueue tr))))
              (recur (inc i)))))

        (while (not (.isEmpty active-q))
          (let [u (int (.poll active-q))]
            (aset in-queue u false)
            (discharge u)))

        (println (aget excess t))))))
