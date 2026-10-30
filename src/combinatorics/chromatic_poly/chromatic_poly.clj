;; chromatic-poly: chromatic polynomial via deletion-contraction.
;; Input: n m k then edges. Output: P(G, k).
(ns chromatic-poly (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] t)
(defn as-long [t] (Long/parseLong t))
(defn ipow [b n] (loop [i 0 r 1] (if (>= i n) r (recur (inc i) (* r b)))))
(defn chrom [n edges k]
  (let [es (sort (set (map (fn [[a b]] [(min a b) (max a b)]) (filter (fn [[a b]] (not= a b)) edges))))]
    (if (empty? es) (ipow k n)
      (let [[u v] (first es)
            rest (vec (rest es))
            p1 (chrom n rest k)
            mp (loop [i 0 nxt 0 m (vec (repeat n 0))]
                 (if (>= i n) m
                   (if (= i v) (recur (inc i) nxt m) (recur (inc i) (inc nxt) (assoc m i nxt)))))
            ne (vec (for [[a b] rest
                          :let [a2 (if (= a v) (nth mp u) (nth mp a))
                                b2 (if (= b v) (nth mp u) (nth mp b))]
                          :when (not= a2 b2)]
                      [a2 b2]))]
        (- p1 (chrom (dec n) ne k))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 3)
      (let [n (as-long (nth toks 0)) m (as-long (nth toks 1)) k (as-long (nth toks 2))
            edges (vec (for [i (range m)] [(as-long (nth toks (+ 3 (* 2 i)))) (as-long (nth toks (+ 4 (* 2 i))))]))]
        (println (chrom n edges k))))))
(-main)
