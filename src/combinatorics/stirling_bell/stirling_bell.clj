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
(defn stirling-row [n k kind]
  (loop [i 0 prev (assoc (vec (repeat (inc k) 0)) 0 1)]
    (if (>= i n) prev
      (let [i1 (inc i)
            cur (loop [j 1 cur (vec (repeat (inc k) 0))]
                  (if (or (> j k) (> j i1)) cur
                    (recur (inc j) (assoc cur j
                      (if (= kind 2)
                        (+ (* j (nth prev j)) (nth prev (dec j)))
                        (+ (* (- i) (nth prev j)) (nth prev (dec j))))))))]
        (recur i1 cur)))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (cond (= "S2" (nth toks 0))
            (let [n (as-long (nth toks 1)) k (as-long (nth toks 2))]
              (println (nth (stirling-row n k 2) k)))
            (= "S1" (nth toks 0))
            (let [n (as-long (nth toks 1)) k (as-long (nth toks 2))]
              (println (nth (stirling-row n k 1) k)))
            :else
            (let [n (as-long (nth toks 1))]
              (println (reduce + (stirling-row n n 2))))))))
(-main)
