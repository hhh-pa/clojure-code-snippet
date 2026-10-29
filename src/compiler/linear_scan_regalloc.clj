(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (java.util.Scanner. ^String s)
          n (.nextInt toks)
          k-regs (.nextInt toks)
          intervals (vec (for [_ (range n)]
                           {:id (.nextInt toks)
                            :start (.nextInt toks)
                            :end (.nextInt toks)}))
          sorted-intervals (sort-by (juxt :start :end) intervals)]
      (loop [remaining sorted-intervals
             free-regs (into (sorted-set) (range k-regs))
             active [] ; vector of {:end, :id, :reg} sorted by :end
             assigned-reg (int-array n -1)
             is-spilled (boolean-array n false)]
        (if (seq remaining)
          (let [cur (first remaining)
                cur-start (:start cur)
                ;; Expire old intervals
                expired (filter #(< (:end %) cur-start) active)
                new-free-regs (into free-regs (map :reg expired))
                unexpired (filter #(>= (:end %) cur-start) active)]
            (if (= (count unexpired) k-regs)
              ;; Spill at interval
              (let [latest (apply max-key :end unexpired)]
                (if (> (:end latest) (:end cur))
                  (do
                    (aset is-spilled (int (:id latest)) true)
                    (aset assigned-reg (int (:id latest)) -1)
                    (aset assigned-reg (int (:id cur)) (int (:reg latest)))
                    (let [new-active (vec (sort-by :end (conj (vec (remove #(= (:id %) (:id latest)) unexpired))
                                                              {:end (:end cur) :id (:id cur) :reg (:reg latest)})))]
                      (recur (rest remaining) new-free-regs new-active assigned-reg is-spilled)))
                  (do
                    (aset is-spilled (int (:id cur)) true)
                    (recur (rest remaining) new-free-regs unexpired assigned-reg is-spilled))))
              ;; Allocate free register
              (let [reg (first new-free-regs)
                    rem-free (disj new-free-regs reg)]
                (aset assigned-reg (int (:id cur)) (int reg))
                (let [new-active (vec (sort-by :end (conj unexpired {:end (:end cur) :id (:id cur) :reg reg})))]
                  (recur (rest remaining) rem-free new-active assigned-reg is-spilled)))))
          ;; Print results
          (let [allocated (volatile! 0)
                spilled (volatile! 0)]
            (dotimes [i n]
              (if (aget is-spilled i)
                (do
                  (println (str "v" i ": SPILL"))
                  (vswap! spilled inc))
                (do
                  (println (str "v" i ": R" (aget assigned-reg i)))
                  (vswap! allocated inc))))
            (println (str "Allocated: " @allocated " Spilled: " @spilled))))))))
