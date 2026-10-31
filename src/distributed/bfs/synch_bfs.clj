(require '[clojure.string :as str])

(defn -main [input]
  (when-not (str/blank? input)
    (let [nums (mapv #(Integer/parseInt %) (str/split (str/trim input) #"\s+"))
          n (nth nums 0) m (nth nums 1)
          edges (partition 2 (subvec nums 2 (+ 2 (* 2 m))))
          root (nth nums (+ 2 (* 2 m)))
          adj (reduce (fn [a [u v]] (-> a (update u (fnil conj []) v) (update v (fnil conj []) u))) (vec (repeat n [])) edges)
          INF 1000000000]
      (loop [front [root] vis (assoc (vec (repeat n false)) root true) par (vec (repeat n -1)) dist (assoc (vec (repeat n INF)) root 0) d 0]
        (if (empty? front)
          (doseq [i (range n)] (println i (nth par i) (let [di (nth dist i)] (if (= di INF) -1 di))))
          (let [[nvis npar nf] (reduce (fn [[vis2 par2 nf2] u] (reduce (fn [[v2 p2 nf3] nb] (if (nth v2 nb) [v2 p2 nf3] [(assoc v2 nb true) (assoc p2 nb u) (conj nf3 nb)])) [vis2 par2 nf2] (get adj u))) [vis par []] front)
                ndist (reduce (fn [d2 u] (assoc d2 u d)) dist front)]
            (recur nf nvis npar ndist (inc d))))))))

(-main (slurp *in*))
