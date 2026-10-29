(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          nt (count toks)
          q (Integer/parseInt (nth toks 0))
          max-lvl 16
          cap (max 20 (+ q 5))
          ^ints val-arr (int-array cap)
          ;; forward table: forward[node * 17 + lvl] = next node (-1 for nil)
          stride 17
          ^ints fwd (int-array (* cap stride) -1)
          node-cnt (int-array [1]) ;; 0 is head
          cur-lvl (int-array [0])
          rng (java.util.Random. 42)
          sb (StringBuilder.)]

      (letfn [(rand-level []
                (loop [lvl 0]
                  (if (and (.nextBoolean rng) (< lvl max-lvl))
                    (recur (inc lvl))
                    lvl)))

              (insert [val]
                (let [update (int-array (inc max-lvl))
                      clvl (aget cur-lvl 0)]
                  (loop [i clvl, cur 0]
                    (if (>= i 0)
                      (let [nxt-cur (loop [c cur]
                                      (let [nxt (aget fwd (+ (* c stride) i))]
                                        (if (and (not= nxt -1) (< (aget val-arr nxt) val))
                                          (recur nxt)
                                          c)))]
                        (aset update i nxt-cur)
                        (recur (dec i) nxt-cur))
                      nil))
                  (let [c0 (aget update 0)
                        first-nxt (aget fwd (+ (* c0 stride) 0))]
                    (when-not (and (not= first-nxt -1) (== (aget val-arr first-nxt) val))
                      (let [rlvl (rand-level)]
                        (when (> rlvl (aget cur-lvl 0))
                          (loop [i (inc (aget cur-lvl 0))]
                            (when (<= i rlvl)
                              (aset update i 0)
                              (recur (inc i))))
                          (aset cur-lvl 0 rlvl))
                        (let [new-idx (aget node-cnt 0)]
                          (aset node-cnt 0 (inc new-idx))
                          (aset val-arr new-idx val)
                          (loop [i 0]
                            (when (<= i rlvl)
                              (let [up-i (aget update i)
                                    nxt (aget fwd (+ (* up-i stride) i))]
                                (aset fwd (+ (* new-idx stride) i) nxt)
                                (aset fwd (+ (* up-i stride) i) new-idx))
                              (recur (inc i))))))))))

              (search [val]
                (let [clvl (aget cur-lvl 0)]
                  (let [cur (loop [i clvl, c 0]
                              (if (>= i 0)
                                (let [nc (loop [k c]
                                           (let [nxt (aget fwd (+ (* k stride) i))]
                                             (if (and (not= nxt -1) (< (aget val-arr nxt) val))
                                               (recur nxt)
                                               k)))]
                                  (recur (dec i) nc))
                                c))
                        target (aget fwd (+ (* cur stride) 0))]
                    (and (not= target -1) (== (aget val-arr target) val)))))

              (remove-val [val]
                (let [update (int-array (inc max-lvl))
                      clvl (aget cur-lvl 0)]
                  (loop [i clvl, cur 0]
                    (if (>= i 0)
                      (let [nxt-cur (loop [c cur]
                                      (let [nxt (aget fwd (+ (* c stride) i))]
                                        (if (and (not= nxt -1) (< (aget val-arr nxt) val))
                                          (recur nxt)
                                          c)))]
                        (aset update i nxt-cur)
                        (recur (dec i) nxt-cur))
                      nil))
                  (let [c0 (aget update 0)
                        target (aget fwd (+ (* c0 stride) 0))]
                    (when (and (not= target -1) (== (aget val-arr target) val))
                      (loop [i 0]
                        (when (<= i (aget cur-lvl 0))
                          (let [up-i (aget update i)]
                            (when (== (aget fwd (+ (* up-i stride) i)) target)
                              (aset fwd (+ (* up-i stride) i) (aget fwd (+ (* target stride) i)))))
                          (recur (inc i))))
                      (loop []
                        (when (and (> (aget cur-lvl 0) 0)
                                   (== (aget fwd (+ (* 0 stride) (aget cur-lvl 0))) -1))
                          (aset cur-lvl 0 (dec (aget cur-lvl 0)))
                          (recur)))))))

              (print-all []
                (loop [cur (aget fwd (+ (* 0 stride) 0))
                       first? true]
                  (when (not= cur -1)
                    (if first?
                      (.append sb (str (aget val-arr cur)))
                      (.append sb (str " " (aget val-arr cur))))
                    (recur (aget fwd (+ (* cur stride) 0)) false)))
                (.append sb "\n"))]

        (loop [i 0, ti 1]
          (when (and (< i q) (< ti nt))
            (let [op (nth toks ti)]
              (cond
                (= op "INSERT")
                (let [val (Integer/parseInt (nth toks (+ ti 1)))]
                  (insert val)
                  (recur (inc i) (+ ti 2)))
                (= op "SEARCH")
                (let [val (Integer/parseInt (nth toks (+ ti 1)))]
                  (.append sb (if (search val) "FOUND\n" "NOT_FOUND\n"))
                  (recur (inc i) (+ ti 2)))
                (= op "DELETE")
                (let [val (Integer/parseInt (nth toks (+ ti 1)))]
                  (remove-val val)
                  (recur (inc i) (+ ti 2)))
                (= op "PRINT")
                (do
                  (print-all)
                  (recur (inc i) (inc ti)))
                :else
                (recur (inc i) (inc ti))))))
        (print (.toString sb))))))
