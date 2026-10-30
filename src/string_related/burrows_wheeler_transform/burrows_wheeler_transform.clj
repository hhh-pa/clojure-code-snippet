(ns solution (:require [clojure.string :as str]))
(defn -main []
  (let [input (slurp *in*) toks (filter seq (str/split input #"\s+")) s (first toks)]
    (when (seq s)
      (let [t (str s "$") n (count t)
            sa (sort-by #(subs t %) (range n))
            bwt (apply str (map #(nth t (mod (dec %) n)) sa))]
        (println bwt)))))
(-main)
