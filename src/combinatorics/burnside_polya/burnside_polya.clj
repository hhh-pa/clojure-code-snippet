;; burnside-polya: necklace and bracelet counts via Burnside's lemma.
;; Modes N n k / B n k.
(ns burnside-polya (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] t)
(defn as-long [t] (Long/parseLong t))
(defn phi [n]
  (loop [p 2 r n x n]
    (if (> (* p p) x) (if (> x 1) (- r (quot r x)) r)
      (if (zero? (mod x p))
        (let [x2 (loop [x x] (if (zero? (mod x p)) (recur (quot x p)) x))]
          (recur (inc p) (- r (quot r p)) x2))
        (recur (inc p) r x)))))
(defn ipow [b e]
  (loop [r 1 b b e e]
    (if (zero? e) r
      (recur (if (odd? e) (* r b) r) (* b b) (quot e 2)))))
(defn necklaces [n k]
  (quot (reduce + (map (fn [d] (if (zero? (mod n d)) (* (phi d) (ipow k (quot n d))) 0)) (range 1 (inc n)))) n))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 3)
      (let [n (as-long (nth toks 1)) k (as-long (nth toks 2)) N (necklaces n k)]
        (if (= "N" (nth toks 0)) (println N)
          (if (odd? n) (println (quot (+ N (ipow k (quot (inc n) 2))) 2))
            (println (quot (+ (* 2 N) (ipow k (inc (quot n 2))) (ipow k (quot n 2))) 4))))))))
(-main)
