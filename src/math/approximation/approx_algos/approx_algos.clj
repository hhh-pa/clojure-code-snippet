;; approx-algos: 2-approx vertex cover, greedy set cover,
;; nearest-neighbor TSP, subset-sum FPTAS. Mode VC/SC/TSP/SS.
(ns approx-algos (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] t)
(defn as-long [t] (Long/parseLong t))
(defn as-double [t] (Double/parseDouble t))
(defn vc-solve [n edges]
  (loop [i 0 cov #{}]
    (if (>= i (count edges)) (sort cov)
      (let [[u v] (nth edges i)]
        (if (and (not (cov u)) (not (cov v)))
          (recur (inc i) (conj cov u v))
          (recur (inc i) cov))))))
(defn sc-solve [n sets]
  (loop [unc (set (range n)) chosen []]
    (if (empty? unc) chosen
      (let [[best bi] (loop [i 0 best -1 bi 0]
                        (if (>= i (count sets)) [best bi]
                          (let [c (count (filter unc (nth sets i)))]
                            (if (> c best) (recur (inc i) c i) (recur (inc i) best bi)))))]
        (if (<= best 0) chosen
          (recur (apply disj unc (nth sets bi)) (conj chosen bi)))))))
(defn tsp-solve [n dist]
  (loop [tour [0] vis #{0} cur 0 total 0]
    (if (>= (count tour) n)
      [(conj tour 0) (+ total (nth (nth dist cur) 0))]
      (let [[best bi] (loop [j 0 best -1 bi 0]
                        (if (>= j n) [best bi]
                          (if (or (vis j)) (recur (inc j) best bi)
                            (let [d (nth (nth dist cur) j)]
                              (if (or (neg? best) (< d best)) (recur (inc j) d j) (recur (inc j) best bi))))))]
        (recur (conj tour bi) (conj vis bi) bi (+ total best))))))
(defn trim-list [L delta]
  (loop [i 1 res [(nth L 0)] last (nth L 0)]
    (if (>= i (count L)) res
      (let [y (nth L i)]
        (if (> y (* last (+ 1 delta))) (recur (inc i) (conj res y) y) (recur (inc i) res last))))))
(defn ss-solve [vals target eps]
  (let [n (count vals) delta (/ eps (* 2 n))]
    (loop [i 0 L [0]]
      (if (>= i n) (last L)
        (let [x (nth vals i)
              M (sort (set (concat L (filter #(<= % target) (map #(+ % x) L)))))]
          (recur (inc i) (trim-list M delta)))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (cond (= "VC" (nth toks 0))
            (let [n (as-long (nth toks 1)) m (as-long (nth toks 2))
                  edges (vec (for [i (range m)] [(as-long (nth toks (+ 3 (* 2 i)))) (as-long (nth toks (+ 4 (* 2 i))))]))
                  out (vc-solve n edges)]
              (println (count out))
              (println (str/join " " (map str out))))
            (= "SC" (nth toks 0))
            (let [n (as-long (nth toks 1)) m (as-long (nth toks 2))
                  [sets _] (loop [i 0 p 3 sets []]
                             (if (>= i m) [sets p]
                               (let [k (as-long (nth toks p))
                                     st (vec (for [j (range k)] (as-long (nth toks (+ p 1 j)))))]
                                 (recur (inc i) (+ p 1 k) (conj sets st)))))
                  out (sc-solve n sets)]
              (println (count out))
              (println (str/join " " (map str out))))
            (= "TSP" (nth toks 0))
            (let [n (as-long (nth toks 1))
                  dist (vec (for [i (range n)] (vec (for [j (range n)] (as-long (nth toks (+ 2 (* i n) j)))))))
                  [tour total] (tsp-solve n dist)]
              (println (str/join " " (map str tour)))
              (println total))
            (= "SS" (nth toks 0))
            (let [n (as-long (nth toks 1)) target (as-long (nth toks 2)) eps (as-double (nth toks 3))
                  vals (vec (for [i (range n)] (as-long (nth toks (+ 4 i)))))]
              (println (ss-solve vals target eps)))))))
(-main)
