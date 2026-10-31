(ns horner
  (:require [clojure.string :as str]))

(def MOD 1000000007)
(defn mod-norm [x] (let [r (mod x MOD)] (if (< r 0) (+ r MOD) r)))

(defn -main []
  (let [tokens (filterv #(not (str/blank? %)) (str/split (str/trim (slurp *in*)) #"\s+"))
        n (when (seq tokens) (try (Integer/parseInt (first tokens)) (catch Exception _ nil)))]
    (when (not (nil? n))
      (let [c (mapv #(mod-norm (Long/parseLong %)) (subvec tokens 1 (inc (+ 1 n))))
            q (Integer/parseInt (nth tokens (+ 2 n)))
            xs (mapv #(mod-norm (Long/parseLong %)) (subvec tokens (+ 3 n) (+ 3 n q)))]
        (println (str/join " " (for [x xs]
                                 (reduce (fn [acc ci] (mod (+ (mod (* acc x) MOD) ci) MOD))
                                         (nth c n) (reverse (subvec c 0 n))))))))))

(-main)
