
;; expectiminimax: game-tree evaluation with max (X), min (N) and chance (C)
;; levels. Chance nodes average their children; exact rational arithmetic
;; with reduced fractions, ties preferring the left child.
(ns expectiminimax
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn gcd* [a b]
  (let [a (if (neg? a) (- a) a) b (if (neg? b) (- b) b)]
    (loop [x a y b]
      (if (zero? y) (if (zero? x) 1 x) (recur y (rem x y))))))

(defn norm* [p q]
  (let [[p q] (if (neg? q) [(- p) (- q)] [p q])
        g (gcd* p q)]
    [(quot p g) (quot q g)]))

(defn cmpf* [[p1 q1] [p2 q2]]
  (compare (* (bigint p1) (bigint q2)) (* (bigint p2) (bigint q1))))

(declare solve*)

(defn solve* [leaves types idx level]
  (if (= level (count types))
    (norm* (nth leaves idx) 1)
    (let [a (solve* leaves types (* idx 2) (inc level))
          b (solve* leaves types (inc (* idx 2)) (inc level))
          t (nth types level)]
      (cond (= t \X) (if (>= (cmpf* a b) 0) a b)
            (= t \N) (if (<= (cmpf* a b) 0) a b)
            :else (let [[p1 q1] a [p2 q2] b]
                    (norm* (+ (* p1 q2) (* p2 q1)) (* 2 q1 q2)))))))

(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (loop [pos 1 tcases (parse-token (first toks)) sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [d (int (parse-token (nth toks pos)))
              types (vec (nth toks (inc pos)))
              n (bit-shift-left 1 d)
              leaves (vec (map parse-token (subvec toks (+ pos 2) (+ pos 2 n))))
              [p q] (solve* leaves types 0 0)]
          (.append sb (str "EXPECTI " p "/" q "\n"))
          (recur (+ pos 2 n) (dec tcases) sb))))))

(-main)
