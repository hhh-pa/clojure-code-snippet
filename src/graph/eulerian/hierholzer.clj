(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          n (Integer/parseInt (nth toks 0))
          m (Integer/parseInt (nth toks 1))
          adj (make-array Object n)
          _ (loop [i 0] (when (< i n) (aset adj i (java.util.ArrayList.)) (recur (inc i))))
          ^ints in-deg (int-array n)
          ^ints out-deg (int-array n)]

      (loop [i 0, ti 2]
        (when (< i m)
          (let [u (Integer/parseInt (nth toks ti))
                v (Integer/parseInt (nth toks (+ ti 1)))]
            (.add ^java.util.ArrayList (aget adj u) v)
            (aset out-deg u (inc (aget out-deg u)))
            (aset in-deg v (inc (aget in-deg v))))
          (recur (inc i) (+ ti 2))))

      (loop [i 0]
        (when (< i n)
          (java.util.Collections/sort ^java.util.ArrayList (aget adj i) (java.util.Collections/reverseOrder))
          (recur (inc i))))

      (let [deg-check
            (loop [i 0, s-node -1, e-node -1, bad 0]
              (if (< i n)
                (let [diff (- (aget out-deg i) (aget in-deg i))]
                  (cond
                    (== diff 1)
                    (recur (inc i) (if (not= s-node -1) 100 i) e-node bad)
                    (== diff -1)
                    (recur (inc i) s-node (if (not= e-node -1) 100 i) bad)
                    (not= diff 0)
                    (recur (inc i) s-node e-node 100)
                    :else
                    (recur (inc i) s-node e-node bad)))
                [s-node e-node bad]))
            [start-node end-node bad-nodes] deg-check]

        (let [[valid? is-circuit? start]
              (cond
                (and (== bad-nodes 0) (== start-node -1) (== end-node -1))
                (let [s (loop [i 0]
                          (if (< i n)
                            (if (> (aget out-deg i) 0) i (recur (inc i)))
                            0))]
                  [true true s])

                (and (== bad-nodes 0) (not= start-node -1) (not= end-node -1))
                [true false start-node]

                :else
                [false false 0])]

          (if-not valid?
            (println "NO EULERIAN PATH")
            (let [st (java.util.ArrayDeque.)
                  path (java.util.ArrayList.)]
              (.push st start)
              (while (not (.isEmpty st))
                (let [u (int (.peek st))
                      ^java.util.ArrayList u-adj (aget adj u)]
                  (if-not (.isEmpty u-adj)
                    (let [v (int (.remove u-adj (dec (.size u-adj))))]
                      (.push st v))
                    (do
                      (.add path u)
                      (.pop st)))))

              (java.util.Collections/reverse path)

              (if (not= (.size path) (inc m))
                (println "NO EULERIAN PATH")
                (do
                  (if is-circuit?
                    (println "CIRCUIT")
                    (println "PATH"))
                  (let [sb (StringBuilder.)
                        sz (.size path)]
                    (loop [i 0]
                      (when (< i sz)
                        (when (> i 0) (.append sb " "))
                        (.append sb (str (.get path i)))
                        (recur (inc i))))
                    (println (.toString sb))))))))))))
