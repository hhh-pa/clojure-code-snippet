(require '[clojure.set :as set])

(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (java.util.Scanner. ^String s)
          n (.nextInt toks)
          m (.nextInt toks)
          adj (vec (repeatedly (inc n) #(java.util.ArrayList.)))
          rev-adj (vec (repeatedly (inc n) #(java.util.ArrayList.)))]
      (dotimes [_ m]
        (let [u (.nextInt toks)
              v (.nextInt toks)]
          (.add ^java.util.ArrayList (nth adj u) v)
          (.add ^java.util.ArrayList (nth rev-adj v) u)))
      (let [all-nodes (into #{} (range 1 (inc n)))
            dom (make-array Object (inc n))]
        (aset dom 1 #{1})
        (doseq [i (range 2 (inc n))]
          (aset dom i all-nodes))
        (loop []
          (let [changed (volatile! false)]
            (doseq [i (range 2 (inc n))]
              (let [preds (vec (nth rev-adj i))]
                (when (seq preds)
                  (let [new-dom (conj (reduce (fn [acc p]
                                                (set/intersection acc (aget dom (int p))))
                                              (aget dom (int (first preds)))
                                              (rest preds))
                                      i)]
                    (when (not= new-dom (aget dom i))
                      (aset dom i new-dom)
                      (vreset! changed true))))))
            (when @changed (recur))))
        (let [idom (int-array (inc n) 0)]
          (doseq [i (range 2 (inc n))
                  :let [dom-i (aget dom i)
                        sz (count dom-i)]]
            (doseq [d dom-i]
              (when (and (not= d i)
                         (= sz (inc (count (aget dom (int d))))))
                (aset idom i (int d)))))
          (let [df (vec (repeatedly (inc n) #(java.util.TreeSet.)))]
            (doseq [x (range 1 (inc n))]
              (doseq [y (nth adj x)]
                (when (not= (aget idom (int y)) x)
                  (.add ^java.util.TreeSet (nth df x) (int y)))))
            (doseq [x (range 1 (inc n))]
              (doseq [z (range 1 (inc n))]
                (when (= (aget idom (int z)) x)
                  (doseq [y (nth df z)]
                    (when (not= (aget idom (int y)) x)
                      (.add ^java.util.TreeSet (nth df x) (int y)))))))
            (let [def-count (.nextInt toks)
                  defs (into (sorted-set) (repeatedly def-count #(.nextInt toks)))
                  phi-blocks (java.util.TreeSet.)
                  w (java.util.ArrayDeque. defs)]
              (while (not (.isEmpty w))
                (let [x (int (.pop w))]
                  (doseq [y (nth df x)]
                    (when (not (.contains phi-blocks (int y)))
                      (.add phi-blocks (int y))
                      (when (not (contains? defs y))
                        (.push w (int y)))))))
              ;; Output idom
              (doseq [i (range 2 (inc n))]
                (print (str "idom(" i ")=" (aget idom i) " ")))
              (println)
              ;; Output phi-blocks
              (if (.isEmpty phi-blocks)
                (println "NONE")
                (println (clojure.string/join " " phi-blocks))))))))))
