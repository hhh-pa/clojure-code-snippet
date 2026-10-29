(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          nt (count toks)
          n-acceptors (Integer/parseInt (nth toks 0))
          n-ops (Integer/parseInt (nth toks 1))
          ^ints promised-n (int-array n-acceptors)
          ^ints accepted-n (int-array n-acceptors)
          accepted-val (make-array String n-acceptors)
          _ (loop [i 0] (when (< i n-acceptors) (aset accepted-val i "") (recur (inc i))))
          majority (inc (quot n-acceptors 2))
          sb (StringBuilder.)]

      (loop [op-idx 0, ti 2]
        (when (and (< op-idx n-ops) (< ti nt))
          (let [op (nth toks ti)]
            (cond
              (= op "PREPARE")
              (let [n-prop (Integer/parseInt (nth toks (+ ti 1)))]
                (let [res (loop [i 0, promises 0, max-acc 0, val-for-max ""]
                            (if (< i n-acceptors)
                              (if (> n-prop (aget promised-n i))
                                (do
                                  (aset promised-n i n-prop)
                                  (let [acc-n (aget accepted-n i)]
                                    (if (> acc-n max-acc)
                                      (recur (inc i) (inc promises) acc-n (aget accepted-val i))
                                      (recur (inc i) (inc promises) max-acc val-for-max))))
                                (recur (inc i) promises max-acc val-for-max))
                              [promises max-acc val-for-max]))
                      [promises max-acc val-for-max] res]
                  (if (>= promises majority)
                    (if (> max-acc 0)
                      (.append sb (str "PROMISED " n-prop " PREV " max-acc " " val-for-max "\n"))
                      (.append sb (str "PROMISED " n-prop " NONE\n")))
                    (.append sb (str "PREPARE REJECTED " n-prop "\n"))))
                (recur (inc op-idx) (+ ti 2)))

              (= op "PROPOSE")
              (let [n-prop (Integer/parseInt (nth toks (+ ti 1)))
                    val (nth toks (+ ti 2))
                    accepted-cnt (loop [i 0, cnt 0]
                                   (if (< i n-acceptors)
                                     (if (>= n-prop (aget promised-n i))
                                       (do
                                         (aset promised-n i n-prop)
                                         (aset accepted-n i n-prop)
                                         (aset accepted-val i val)
                                         (recur (inc i) (inc cnt)))
                                       (recur (inc i) cnt))
                                     cnt))]
                (if (>= accepted-cnt majority)
                  (.append sb (str "CHOSEN " n-prop " " val "\n"))
                  (.append sb (str "PROPOSE REJECTED " n-prop "\n")))
                (recur (inc op-idx) (+ ti 3)))

              :else
              (recur (inc op-idx) (inc ti))))))
      (print (.toString sb)))))
