(ns solution (:require [clojure.string :as str]))
(defn overlap [a b]
  (let [mn (min (count a) (count b))]
    (or (first (filter #(= (subs a (- (count a) %)) (subs b 0 %)) (range mn 0 -1))) 0)))
(defn -main []
  (let [input (slurp *in*) toks (filter seq (str/split input #"\s+"))
        n (when (seq toks) (Integer/parseInt (first toks)))
        strs (vec (take n (rest toks)))]
    (when (seq strs)
      (loop [strs strs]
        (if (<= (count strs) 1) (println (first strs))
          (let [pairs (for [i (range (count strs)) j (range (count strs)) :when (not= i j)] [i j])
                scored (map (fn [[i j]] [i j (overlap (nth strs i) (nth strs j))]) pairs)
                [bi bj bov] (reduce (fn [best cur] (if (> (nth cur 2) (nth best 2)) cur best)) scored)
                merged (str (nth strs bi) (subs (nth strs bj) bov))
                next (conj (vec (keep-indexed (fn [k s] (when (and (not= k bi) (not= k bj)) s)) strs)) merged)]
            (recur next)))))))
(-main)
