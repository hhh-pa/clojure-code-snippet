;; montgomery: Montgomery multiplication with REDC (R = 256, odd m < 256).
;; Input: a b m. Output: aM bM in Montgomery form, product in
;; Montgomery form, product converted back.
(ns montgomery (:require [clojure.string :as str]))
(def ^:const R 256)
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Long/parseLong t))
(defn redc [T m mp]
  (let [q (mod (* (mod T R) mp) R)
        t (quot (+ T (* q m)) R)]
    (if (>= t m) (- t m) t)))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 3)
      (let [vals (mapv parse-token toks)
            a (nth vals 0) b (nth vals 1) m (nth vals 2)
            minv (first (filter #(= 1 (mod (* m %) R)) (range 1 R)))
            mp (mod (- R minv) R)
            aM (mod (* (mod a m) R) m)
            bM (mod (* (mod b m) R) m)
            abM (redc (* aM bM) m mp)]
        (println aM bM)
        (println abM)
        (println (redc abM m mp))))))
(-main)
