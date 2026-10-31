;; sjt: Steinhaus-Johnson-Trotter mobile-element permutation generation.
;; Input: n. Output: all n! permutations, one per line.
(ns sjt (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Long/parseLong t))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (let [n (parse-token (nth toks 0))]
        (loop [p (vec (range n)) d (vec (repeat n -1))]
          (println (str/join " " (map str p)))
          (let [[mob mobpos] (loop [i 0 mob -1 mobpos -1]
                               (if (>= i n) [mob mobpos]
                                 (let [v (nth p i) j (+ i (nth d v))]
                                   (if (and (<= 0 j) (< j n) (< (nth p j) v) (> v mob))
                                     (recur (inc i) v i)
                                     (recur (inc i) mob mobpos)))))]
            (when (>= mob 0)
              (let [j (+ mobpos (nth d mob))
                    p2 (assoc p mobpos (nth p j) j (nth p mobpos))
                    d2 (mapv (fn [v dd] (if (> v mob) (- dd) dd)) (range n) d)]
                (recur p2 d2)))))))))
(-main)
