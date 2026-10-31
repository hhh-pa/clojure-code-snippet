
;; adaboost: AdaBoost ensemble learning with threshold stumps on
;; integer features. Exponential sample reweighting, weighted majority
;; vote; stops on perfect or chance-level weak learners.
(ns adaboost
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn stump-pred [x f t pol]
  (if (= (>= (nth x f) t) (= pol 1)) 1 0))

(defn solve-one [n f r x y]
  (loop [rd 0 w (vec (repeat n (/ 1.0 n))) ens []]
    (if (= rd r)
      ens
      (let [[bf bt bpol be]
            (reduce (fn [[bf bt bp be] feat]
                      (let [vals (sort (distinct (map #(nth % feat) x)))]
                        (reduce (fn [[bf2 bt2 bp2 be2] th]
                                  (reduce (fn [[bf3 bt3 bp3 be3] pol]
                                            (let [e (reduce + 0.0
                                                            (for [i (range n)
                                                                  :when (not= (stump-pred (nth x i) feat th pol)
                                                                              (nth y i))]
                                                              (nth w i)))]
                                              (if (< e (- be3 1e-12)) [feat th pol e] [bf3 bt3 bp3 be3])))
                                          [bf2 bt2 bp2 be2] [0 1]))
                                [bf bt bp be] vals)))
                    [0 0 0 1e300] (range f))]
        (if (>= be 0.5)
          ens
          (let [alpha (if (<= be 0) 10.0 (* 0.5 (Math/log (/ (- 1.0 be) be))))
                [w2 tot] (reduce (fn [[ww tt] i]
                                   (let [h (if (= (stump-pred (nth x i) bf bt bpol) 1) 1.0 -1.0)
                                         yy (if (= (nth y i) 1) 1.0 -1.0)
                                         nw (* (nth ww i) (Math/exp (* (- alpha) yy h)))]
                                     [(assoc ww i nw) (+ tt nw)]))
                                 [w 0.0] (range n))
                w3 (mapv #(/ % tot) w2)
                ens2 (conj ens [bf bt bpol alpha])]
            (if (<= be 0)
              ens2
              (recur (inc rd) w3 ens2))))))))

(defn predict-all [x ens]
  (mapv (fn [xi]
          (let [s (reduce + 0.0 (for [[f th pol al] ens]
                                   (* al (if (= (stump-pred xi f th pol) 1) 1.0 -1.0))))]
            (if (>= s 0) 1 0)))
        x))

(defn parse-case [nums pos]
  (let [n (int (nth nums pos)) f (int (nth nums (+ pos 1))) r (int (nth nums (+ pos 2)))
        p0 (+ pos 3)
        x (vec (for [i (range n)] (vec (map int (subvec nums (+ p0 (* i (inc f))) (+ p0 (* i (inc f)) f))))))
        y (vec (for [i (range n)] (int (nth nums (+ p0 (* i (inc f)) f)))))]
    [n f r x y (+ p0 (* n (inc f)))]))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[n f r x y p2] (parse-case nums pos)
              ens (solve-one n f r x y)
              preds (predict-all x ens)
              err (count (filter true? (map not= preds y)))]
          (.append sb (str "ADA " (count ens) " " err "\n"))
          (.append sb (str (str/join " " preds) "\n"))
          (recur p2 (dec tcases) sb))))))

(-main)
