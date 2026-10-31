;; perm-rank: permutation ranking/unranking via Lehmer code.
;; Modes R n perm (rank + Lehmer code) / U n rank (perm).
(ns perm-rank (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] t)
(defn as-long [t] (Long/parseLong t))
(defn fact [n] (loop [i 2 f 1] (if (> i n) f (recur (inc i) (* f i)))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (if (= "R" (nth toks 0))
        (let [n (as-long (nth toks 1))
              p (mapv as-long (take n (drop 2 toks)))
              L (mapv (fn [i] (count (filter #(> (nth p i) %) (subvec p (inc i))))) (range n))
              rank (reduce + (map * L (map fact (map #(- n 1 %) (range n)))))]
          (println rank)
          (println (str/join " " (map str L))))
        (let [n (as-long (nth toks 1)) r (as-long (nth toks 2))]
          (println (str/join " " (map str
            (loop [i 0 rr r avail (vec (range n)) acc []]
              (if (>= i n) acc
                (let [f (fact (- n 1 i)) idx (quot rr f)]
                  (recur (inc i) (mod rr f)
                         (vec (concat (subvec avail 0 idx) (subvec avail (inc idx))))
                         (conj acc (nth avail idx))))))))))))))
(-main)
