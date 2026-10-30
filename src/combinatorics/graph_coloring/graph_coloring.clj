;; graph-coloring: greedy coloring and Welsh-Powell ordering.
;; Input: mode n m then edges. Output: #colors, color per vertex.
(ns graph-coloring (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] t)
(defn as-long [t] (Long/parseLong t))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 3)
      (let [n (as-long (nth toks 1)) m (as-long (nth toks 2))
            edges (vec (for [i (range m)] [(as-long (nth toks (+ 3 (* 2 i)))) (as-long (nth toks (+ 4 (* 2 i))))]))
            adj (loop [i 0 a (vec (repeat n []))]
                  (if (>= i m) a
                    (let [[u v] (nth edges i)]
                      (recur (inc i) (assoc a u (conj (nth a u) v) v (conj (nth a v) u))))))
            deg (mapv count adj)
            order (if (= "W" (nth toks 0))
                      (sort-by (fn [u] [(- (nth deg u)) u]) (range n))
                      (range n))
            color (loop [os order c (vec (repeat n -1))]
                    (if (empty? os) c
                      (let [u (first os)
                            used (set (filter #(>= % 0) (map #(nth c %) (nth adj u))))
                            cc (loop [cc 0] (if (used cc) (recur (inc cc)) cc))]
                        (recur (rest os) (assoc c u cc)))))]
        (println (inc (apply max color)))
        (println (str/join " " (map str color)))))))
(-main)
