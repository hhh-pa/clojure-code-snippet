(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          nt (count toks)
          n-procs (Integer/parseInt (nth toks 0))
          n-ops (Integer/parseInt (nth toks 1))
          vc (to-array (map (fn [_] (int-array n-procs)) (range n-procs)))
          event-clocks (java.util.HashMap.)
          in-flight (java.util.HashMap.)
          sb (StringBuilder.)]

      (letfn [(copy-arr [^ints a]
                (let [cp (int-array n-procs)]
                  (System/arraycopy a 0 cp 0 n-procs)
                  cp))
              (compare-clocks [^ints a ^ints b]
                (let [res (loop [i 0, aleb true, blea true]
                            (if (< i n-procs)
                              (let [ai (aget a i)
                                    bi (aget b i)]
                                (recur (inc i)
                                       (if (> ai bi) false aleb)
                                       (if (> bi ai) false blea)))
                              [aleb blea]))]
                  (let [[aleb blea] res]
                    (cond
                      (and aleb blea) 2
                      aleb 1
                      blea -1
                      :else 0))))]

        (let [ti-after-ops
              (loop [i 0, ti 2]
                (if (and (< i n-ops) (< ti nt))
                  (let [op (nth toks ti)]
                    (cond
                      (= op "LOCAL")
                      (let [p (Integer/parseInt (nth toks (+ ti 1)))
                            name (nth toks (+ ti 2))
                            ^ints p-vc (aget ^"[[I" vc p)]
                        (aset p-vc p (inc (aget p-vc p)))
                        (.put event-clocks name (copy-arr p-vc))
                        (recur (inc i) (+ ti 3)))

                      (= op "SEND")
                      (let [from-p (Integer/parseInt (nth toks (+ ti 1)))
                            msg-id (nth toks (+ ti 2))
                            ^ints p-vc (aget ^"[[I" vc from-p)]
                        (aset p-vc from-p (inc (aget p-vc from-p)))
                        (let [cp (copy-arr p-vc)]
                          (.put in-flight msg-id cp)
                          (.put event-clocks (str msg-id "_SEND") cp))
                        (recur (inc i) (+ ti 3)))

                      (= op "RECV")
                      (let [to-p (Integer/parseInt (nth toks (+ ti 1)))
                            msg-id (nth toks (+ ti 2))
                            ^ints to-vc (aget ^"[[I" vc to-p)
                            ^ints msg-vc (.get in-flight msg-id)]
                        (when msg-vc
                          (loop [k 0]
                            (when (< k n-procs)
                              (aset to-vc k (max (aget to-vc k) (aget msg-vc k)))
                              (recur (inc k)))))
                        (aset to-vc to-p (inc (aget to-vc to-p)))
                        (.put event-clocks (str msg-id "_RECV") (copy-arr to-vc))
                        (recur (inc i) (+ ti 3)))

                      :else
                      (recur (inc i) (inc ti))))
                  ti))]

          (when (< ti-after-ops nt)
            (let [n-queries (Integer/parseInt (nth toks ti-after-ops))]
              (loop [q 0, ti (inc ti-after-ops)]
                (when (and (< q n-queries) (< (inc ti) nt))
                  (let [e1 (nth toks ti)
                        e2 (nth toks (inc ti))
                        ^ints v1 (or (.get event-clocks e1) (int-array n-procs))
                        ^ints v2 (or (.get event-clocks e2) (int-array n-procs))
                        rel (compare-clocks v1 v2)]
                    (cond
                      (== rel 1) (.append sb (str e1 " -> " e2 "\n"))
                      (== rel -1) (.append sb (str e2 " -> " e1 "\n"))
                      (== rel 2) (.append sb (str e1 " == " e2 "\n"))
                      :else (.append sb (str e1 " || " e2 "\n")))
                    (recur (inc q) (+ ti 2)))))))
          (print (.toString sb)))))))
