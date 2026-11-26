;; Tree diameter in edges (two-pass farthest-node search).
(require '[clojure.string :as str])

(defn- td-bfs
  [adj s]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY s)
         dist (assoc (vec (repeat (count adj) -1)) s 0)]
    (if (empty? queue)
      dist
      (let [u (peek queue)
            [q2 d2] (reduce (fn [[q d] w]
                              (if (neg? (nth d w))
                                [(conj q w) (assoc d w (inc (nth d u)))]
                                [q d]))
                            [(pop queue) dist] (nth adj u))]
        (recur q2 d2)))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          n (nth toks 0)
          adj (reduce (fn [g i]
                        (let [u (nth toks (+ 1 (* 2 i))) v (nth toks (+ 2 (* 2 i)))]
                          (-> g
                              (update u conj v)
                              (update v conj u))))
                      (vec (repeat n [])) (range (dec n)))]
      (if (= n 1)
        (print 0)
        (let [d0 (td-bfs adj 0)
              a (apply max-key #(nth d0 %) (range n))
              d1 (td-bfs adj a)]
          (print (apply max d1)))))))
