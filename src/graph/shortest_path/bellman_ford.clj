(require 'clojure.string)

(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          v (Integer/parseInt (nth toks 0))
          e (Integer/parseInt (nth toks 1))
          s (Integer/parseInt (nth toks 2))
          ^ints eu (int-array e)
          ^ints ev (int-array e)
          ^longs ew (long-array e)
          inf 1000000000000000000
          ^longs dist (long-array v inf)
          ^ints parent (int-array v -1)]
      (loop [i 0, ti 3]
        (when (< i e)
          (aset eu i (Integer/parseInt (nth toks ti)))
          (aset ev i (Integer/parseInt (nth toks (+ ti 1))))
          (aset ew i (Long/parseLong (nth toks (+ ti 2))))
          (recur (inc i) (+ ti 3))))
      (aset dist s 0)
      (let [final-lr (loop [iter 0, prev -1]
                       (if (>= iter v)
                         prev
                         (let [lr (loop [i 0, lr -1]
                                    (if (>= i e)
                                      lr
                                      (let [u (aget eu i)
                                            to-node (aget ev i)
                                            w (aget ew i)
                                            du (aget dist u)]
                                        (if (and (< du inf) (< (+ du w) (aget dist to-node)))
                                          (do
                                            (aset dist to-node (+ du w))
                                            (aset parent to-node u)
                                            (recur (inc i) to-node))
                                          (recur (inc i) lr)))))]
                           (if (== lr -1)
                             -1
                             (recur (inc iter) lr)))))]
        (if (not= final-lr -1)
          (let [cn (loop [i 0, c final-lr]
                     (if (>= i v)
                       c
                       (recur (inc i) (aget parent c))))
                cyc (loop [cur cn, acc []]
                      (let [nacc (conj acc cur)]
                        (if (and (== cur cn) (> (count nacc) 1))
                          nacc
                          (recur (aget parent cur) nacc))))]
            (println "NEGATIVE CYCLE")
            (println (clojure.string/join " " (reverse cyc))))
          (let [half (quot inf 2)]
            (dotimes [i v]
              (if (>= (aget dist i) half)
                (println "INF")
                (println (aget dist i))))))))))
