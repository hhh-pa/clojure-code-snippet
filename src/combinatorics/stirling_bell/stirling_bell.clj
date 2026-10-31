;; stirling-bell: Stirling numbers of both kinds and Bell numbers.
;; Modes S2 n k / S1 n k / B n.
(ns stirling-bell (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] t)
(defn as-long [t] (Long/parseLong t))
(defn stirling-table [n k kind]
  (loop [i 1 T (assoc (vec (repeat (inc n) (vec (repeat (inc k) 0)))) 0 (assoc (vec (repeat (inc k) 0)) 0 1))]
    (if (> i n) T
      (recur (inc i) (loop [j 1 T T]
                       (if (or (> j k) (> j i)) T
                         (recur (inc j) (assoc-in T [i j]
                           (if (= kind 2)
                             (+ (* j (nth (nth T (dec i)) j)) (nth (nth T (dec i)) (dec j)))
                             (+ (* (- (dec i)) (nth (nth T (dec i)) j)) (nth (nth T (dec i)) (dec j))))))))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (cond (= "S2" (nth toks 0))
            (let [n (as-long (nth toks 1)) k (as-long (nth toks 2))]
              (println (nth (nth (stirling-table n k 2) n) k)))
            (= "S1" (nth toks 0))
            (let [n (as-long (nth toks 1)) k (as-long (nth toks 2))]
              (println (nth (nth (stirling-table n k 1) n) k)))
            :else
            (let [n (as-long (nth toks 1))]
              (println (reduce + (nth (stirling-table n n 2) n))))))))
(-main)
