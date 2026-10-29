(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (java.util.Scanner. ^String s)
          n (.nextInt toks)
          m (.nextInt toks)
          k (.nextInt toks)
          adj (vec (map vec (let [a (make-array Object n)]
                              (dotimes [i n] (aset a i (java.util.ArrayList.)))
                              (dotimes [_ m]
                                (let [u (.nextInt toks)
                                      v (.nextInt toks)]
                                  (.add ^java.util.ArrayList (aget a u) v)
                                  (.add ^java.util.ArrayList (aget a v) u)))
                              (map vec a))))]
      (loop [active (into (sorted-set) (range n))
             select-stack ()
             spilled (boolean-array n false)]
        (if (empty? active)
          ;; Color phase
          (let [color (int-array n -1)]
            (loop [stk select-stack]
              (if (empty? stk)
                (dotimes [i n]
                  (if (aget spilled i)
                    (println (str "v" i ":SPILL"))
                    (println (str "v" i ":R" (aget color i)))))
                (let [u (first stk)
                      used (into #{} (keep (fn [v]
                                             (let [c (aget color (int v))]
                                               (when (not= c -1) c)))
                                           (nth adj u)))
                      c (first (filter #(not (contains? used %)) (range k)))]
                  (if c
                    (aset color (int u) (int c))
                    (aset spilled (int u) true))
                  (recur (rest stk))))))
          ;; Select phase
          (let [simplify-node (first (filter (fn [u]
                                               (let [deg (count (filter #(contains? active %) (nth adj u)))]
                                                 (< deg k)))
                                             active))]
            (if simplify-node
              (recur (disj active simplify-node)
                     (conj select-stack simplify-node)
                     spilled)
              (let [spill-node (first active)]
                (aset spilled (int spill-node) true)
                (recur (disj active spill-node)
                       select-stack
                       spilled)))))))))
