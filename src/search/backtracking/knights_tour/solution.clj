;; Knight's tour via Warnsdorff (ties: smallest row, then column).
(require '[clojure.string :as str])

(def ^:private kt-moves [[2 1] [1 2] [-1 2] [-2 1] [-2 -1] [-1 -2] [1 -2] [2 -1]])

(defn- kt-onward [seen n r c]
  (count (for [[dr dc] kt-moves
               :let [nr (+ r dr) nc (+ c dc)]
               :when (and (<= 0 nr (dec n)) (<= 0 nc (dec n)) (not (seen [nr nc])))]
           1)))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          n (nth toks 0) sr (nth toks 1) sc (nth toks 2)]
      (loop [tour [[sr sc]] seen #{[sr sc]}]
        (if (= (count tour) (* n n))
          (print (str/join "\n" (map (fn [[r c]] (str r " " c)) tour)))
          (let [[r c] (peek tour)
                cands (for [[dr dc] kt-moves
                            :let [nr (+ r dr) nc (+ c dc)]
                            :when (and (<= 0 nr (dec n)) (<= 0 nc (dec n))
                                       (not (seen [nr nc])))]
                        [(kt-onward seen n nr nc) nr nc])]
            (if (empty? cands)
              (print "NO TOUR")
              (let [[_ nr nc] (first (sort cands))]
                (recur (conj tour [nr nc]) (conj seen [nr nc]))))))))))
