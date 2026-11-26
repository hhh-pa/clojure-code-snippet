;; undirected cycle detection: DFS with parent tracking, self-loops count.
(require '[clojure.string :as str])

(defn dfs-from [adj seen u p]
  (reduce (fn [[seen found] w]
            (if found
              [seen true]
              (if (contains? seen w)
                (if (= w p) [seen false] [seen true])
                (dfs-from adj (conj seen w) w u))))
          [seen false]
          (adj u)))

(defn has-cycle? [n edges]
  (if (some (fn [[u v]] (= u v)) edges)
    true
    (let [adj (reduce (fn [a [u v]]
                        (if (and (<= 0 u (dec n)) (<= 0 v (dec n)))
                          (-> a (update u conj v) (update v conj u))
                          a))
                      (vec (repeat n []))
                      edges)]
      (loop [seen #{}
             vs (range n)]
        (cond (empty? vs) false
              (contains? seen (first vs)) (recur seen (rest vs))
              :else (let [[seen2 found]
                          (dfs-from adj (conj seen (first vs)) (first vs) -1)]
                      (if found true (recur seen2 (rest vs)))))))))

(defn -main
  "Read n, m, edges and print 1 if any cycle exists, else 0."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))
        nums (mapv #(Long/parseLong %) toks)]
    (when (and (>= (count nums) 2)
               (>= (nth nums 0) 0)
               (>= (nth nums 1) 0))
      (let [n (nth nums 0)
            m (nth nums 1)
            pairs (take m (partition 2 (drop 2 nums)))
            edges (mapv vec pairs)]
        (print (if (has-cycle? n edges) 1 0))))))

(-main (slurp *in*))
