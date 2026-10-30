
;; decision_tree: ID3 decision-tree learning with information gain.
;; Binary features and labels; ties prefer the smallest feature and
;; majority label 1; empty branches inherit the parent majority.
(ns decision-tree
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn entropy* [y idx]
  (if (empty? idx)
    0.0
    (let [c (count (filter #(= 1 (nth y %)) idx))
          p (/ (double c) (double (count idx)))]
      (if (or (zero? p) (= p 1.0))
        0.0
        (- (+ (* p (Math/log p)) (* (- 1.0 p) (Math/log (- 1.0 p)))))))))

(declare build*)

(defn build* [x y f d idx used depth pmaj]
  (let [c1 (count (filter #(= 1 (nth y %)) idx))
        c0 (- (count idx) c1)]
    (cond (empty? idx) {:leaf true :label pmaj :depth depth}
          (or (zero? c0) (zero? c1) (>= depth d) (= used (bit-shift-left 1 f)))
          {:leaf true :label (if (>= c1 c0) 1 0) :depth depth}
          :else
          (let [base (entropy* y idx)
                [bf _] (reduce (fn [[bfeat bg] feat]
                                 (if (= 1 (bit-and (bit-shift-right used feat) 1))
                                   [bfeat bg]
                                   (let [l (filterv #(zero? (nth (nth x %) feat)) idx)
                                         r (filterv #(= 1 (nth (nth x %) feat)) idx)
                                         g (- base
                                              (* (/ (double (count l)) (double (count idx))) (entropy* y l))
                                              (* (/ (double (count r)) (double (count idx))) (entropy* y r)))]
                                     (if (> g (+ bg 1e-12)) [feat g] [bfeat bg]))))
                               [0 -1.0] (range f))
                maj (if (>= c1 c0) 1 0)
                l (filterv #(zero? (nth (nth x %) bf)) idx)
                r (filterv #(= 1 (nth (nth x %) bf)) idx)]
            {:leaf false :feat bf :depth depth
             :l (build* x y f d l (bit-or used (bit-shift-left 1 bf)) (inc depth) maj)
             :r (build* x y f d r (bit-or used (bit-shift-left 1 bf)) (inc depth) maj)}))))

(defn predict* [nd x]
  (loop [n nd]
    (if (:leaf n)
      (:label n)
      (recur (if (zero? (nth x (:feat n))) (:l n) (:r n))))))

(defn maxdepth* [nd]
  (if (:leaf nd)
    (:depth nd)
    (max (maxdepth* (:l nd)) (maxdepth* (:r nd)))))

(defn parse-case [nums pos]
  (let [n (int (nth nums pos)) f (int (nth nums (+ pos 1))) d (int (nth nums (+ pos 2)))
        p0 (+ pos 3)
        x (vec (for [i (range n)] (vec (map int (subvec nums (+ p0 (* i (inc f))) (+ p0 (* i (inc f)) f))))))
        y (vec (for [i (range n)] (int (nth nums (+ p0 (* i (inc f)) f)))))]
    [n f d x y (+ p0 (* n (inc f)))]))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[n f d x y p2] (parse-case nums pos)
              root (build* x y f d (vec (range n)) 0 0 1)]
          (.append sb (str "DT " (maxdepth* root) "\n"))
          (.append sb (str (str/join " " (map #(predict* root (nth x %)) (range n))) "\n"))
          (recur p2 (dec tcases) sb))))))

(-main)
