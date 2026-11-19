;; https://leetcode.com/problems/is-graph-bipartite/
;; two-coloring: self-loop or odd cycle fails, else bipartite.
(require '[clojure.string :as str])

(defn color-from [adj color s]
  (loop [color (assoc color s 0)
         work [s]]
    (if (empty? work)
      [color true]
      (let [u (peek work)
            bad (some (fn [w]
                        (let [c (color w)]
                          (and (not= c -1) (= c (color u)))))
                      (adj u))]
        (if bad
          [color false]
          (let [fresh (remove #(not= (color %) -1) (adj u))]
            (recur (reduce (fn [c w] (assoc c w (- 1 (color u))))
                           color fresh)
                   (into (pop work) fresh))))))))

(defn bipartite? [n edges]
  (if (some (fn [[u v]] (= u v)) edges)
    false
    (let [adj (reduce (fn [a [u v]]
                        (if (and (<= 0 u (dec n)) (<= 0 v (dec n)))
                          (-> a (update u conj v) (update v conj u))
                          a))
                      (vec (repeat n []))
                      edges)]
      (loop [color (vec (repeat n -1))
             vs (range n)]
        (cond (empty? vs) true
              (not= (color (first vs)) -1) (recur color (rest vs))
              :else (let [[color2 ok] (color-from adj color (first vs))]
                      (if ok (recur color2 (rest vs)) false)))))))

(defn -main
  "Read n, m, edges and print 1 iff bipartite, else 0."
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
        (print (if (bipartite? n edges) 1 0))))))

(-main (slurp *in*))
