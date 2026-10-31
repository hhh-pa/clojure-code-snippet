;; gray-code: binary reflected Gray code generation, ranking, unranking.
;; Modes G n / R graystring / U n rank.
(ns gray-code (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 2) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] t)
(defn as-long [t] (Long/parseLong t))
(defn gray-to-bin [g]
  (loop [b g gg (bit-shift-right g 1)]
    (if (zero? gg) b (recur (bit-xor b gg) (bit-shift-right gg 1)))))
(defn fmt-gray [g n]
  (apply str (map (fn [i] (if (zero? (bit-and (bit-shift-right g i) 1)) \0 \1)) (range (dec n) -1 -1))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (cond (= "G" (nth toks 0))
            (let [n (as-long (nth toks 1))]
              (doseq [i (range (bit-shift-left 1 n))]
                (println (fmt-gray (bit-xor i (bit-shift-right i 1)) n))))
            (= "R" (nth toks 0))
            (let [g (nth toks 1)
                  v (reduce + (map * (digit-values g) (map #(bit-shift-left 1 %) (range (dec (count g)) -1 -1))))]
              (println (gray-to-bin v)))
            :else
            (let [n (as-long (nth toks 1)) r (as-long (nth toks 2))]
              (println (fmt-gray (bit-xor r (bit-shift-right r 1)) n)))))))
(-main)
