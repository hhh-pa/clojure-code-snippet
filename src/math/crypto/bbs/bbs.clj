;; bbs: Blum-Blum-Shub pseudorandom generator x <- x^2 mod M.
;; Input: x0 M count. Output: bitstring then final state.
(ns bbs (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Long/parseLong t))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 3)
      (let [vals (mapv parse-token toks)
            M (nth vals 1) cnt (nth vals 2)]
        (loop [i 0 x (nth vals 0) acc []]
          (if (>= i cnt)
            (do (println (apply str acc)) (println x))
            (let [nx (mod (* x x) M)]
              (recur (inc i) nx (conj acc (char (+ 48 (mod nx 2))))))))))))
(-main)
