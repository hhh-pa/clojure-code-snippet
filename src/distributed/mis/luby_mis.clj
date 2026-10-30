(require '[clojure.string :as str])

(defn pri [u] (bit-and (bit-shift-right (* (inc u) 2654435761) 16) 0xFFFFFFFF))
(defn -main [input]
  (when-not (str/blank? input)
    (let [nums (mapv #(Integer/parseInt %) (str/split (str/trim input) #"\s+"))
          n (nth nums 0) m (nth nums 1)
          edges (partition 2 (subvec nums 2 (+ 2 (* 2 m))))
          adj (reduce (fn [a [u v]] (-> a (update u (fnil conj #{}) v) (update v (fnil conj #{}) u))) (vec (repeat n #{})) edges)]
      (loop [rem (vec (repeat n false)) in-mis (vec (repeat n false))]
        (let [cands (filterv (fn [u] (and (not (nth rem u)) (every? (fn [v] (or (nth rem v) (<= (pri v) (pri u)))) (get adj u)))) (range n))]
          (if (empty? cands)
            (let [res (sort (keep-indexed (fn [i v] (when v i)) in-mis))] (println (count res)) (when (seq res) (println (str/join " " res))))
            (let [nrem (reduce (fn [r u] (reduce (fn [r2 v] (assoc r2 v true)) (assoc r u true) (get adj u))) rem cands)
                  nmis (reduce (fn [mm u] (assoc mm u true)) in-mis cands)]
              (recur nrem nmis))))))))

(-main (slurp *in*))
