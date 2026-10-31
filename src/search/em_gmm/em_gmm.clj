
;; em_gmm: expectation-maximization for a 1D two-component Gaussian
;; mixture. E-step soft responsibilities, M-step parameter updates;
;; components ordered by mean for stable output.
(ns em-gmm
  (:require [clojure.string :as str]))

(defn digit-values [s] (mapv #(Long/parseLong %) s))
(defn separators [s] nil)
(defn tokenize [s] (str/split (str/trim s) #"\s+"))
(defn parse-token [t] (Long/parseLong t))

(defn fmt6 [x]
  (String/format java.util.Locale/US "%.6f" (into-array Object [(double x)])))

(defn solve-one [n iters x]
  (let [mn (apply min x) mx (apply max x)
        mean (/ (reduce + 0.0 x) n)
        va0 (/ (reduce + 0.0 (map (fn [v] (* (- v mean) (- v mean))) x)) n)
        va (if (< va0 1e-6) 1e-6 va0)]
    (loop [it 0 w0 0.5 w1 0.5 mu0 mn mu1 mx v0 va v1 va]
      (if (= it iters)
        (let [[w0 w1 mu0 mu1 v0 v1 g0 g1]
              ;; final E-step for assignments
              (let [gs (mapv (fn [xi]
                               (let [p0 (/ (* w0 (Math/exp (/ (* -0.5 (- xi mu0) (- xi mu0)) v0))) (Math/sqrt v0))
                                     p1 (/ (* w1 (Math/exp (/ (* -0.5 (- xi mu1) (- xi mu1)) v1))) (Math/sqrt v1))
                                     s (+ p0 p1)]
                                 (if (zero? s) [0.5 0.5] [(/ p0 s) (/ p1 s)])))
                             x)]
                [w0 w1 mu0 mu1 v0 v1 (mapv first gs) (mapv second gs)])]
          (if (< mu1 mu0)
            [w1 mu1 v1 w0 mu0 v0 g1 g0]
            [w0 mu0 v0 w1 mu1 v1 g0 g1]))
        (let [gs (mapv (fn [xi]
                         (let [p0 (/ (* w0 (Math/exp (/ (* -0.5 (- xi mu0) (- xi mu0)) v0))) (Math/sqrt v0))
                               p1 (/ (* w1 (Math/exp (/ (* -0.5 (- xi mu1) (- xi mu1)) v1))) (Math/sqrt v1))
                               s (+ p0 p1)]
                           (if (zero? s) [0.5 0.5] [(/ p0 s) (/ p1 s)])))
                       x)
              g0 (mapv first gs) g1 (mapv second gs)
              n0 (reduce + 0.0 g0) n1 (reduce + 0.0 g1)
              nw0 (/ n0 n) nw1 (/ n1 n)
              nmu0 (/ (reduce + 0.0 (map * g0 x)) n0)
              nmu1 (/ (reduce + 0.0 (map * g1 x)) n1)
              nv0 (/ (reduce + 0.0 (map (fn [g xi] (* g (- xi nmu0) (- xi nmu0))) g0 x)) n0)
              nv1 (/ (reduce + 0.0 (map (fn [g xi] (* g (- xi nmu1) (- xi nmu1))) g1 x)) n1)]
          (recur (inc it) nw0 nw1 nmu0 nmu1
                 (if (< nv0 1e-6) 1e-6 nv0) (if (< nv1 1e-6) 1e-6 nv1)))))))

(defn parse-case [nums pos]
  (let [n (int (nth nums pos)) it (int (nth nums (+ pos 1)))
        p0 (+ pos 2)
        x (vec (for [i (range n)] (/ (double (nth nums (+ p0 (* i 2)))) (double (nth nums (+ p0 (* i 2) 1))))))]
    [n it x (+ p0 (* n 2))]))

(defn -main []
  (let [nums (digit-values (tokenize (slurp *in*)))
        t (first nums)]
    (loop [pos 1 tcases t sb (StringBuilder.)]
      (if (zero? tcases)
        (print (str sb))
        (let [[n it x p2] (parse-case nums pos)
              [w0 mu0 v0 w1 mu1 v1 g0 g1] (solve-one n it x)]
          (.append sb "EM\n")
          (.append sb (str (str/join " " (map fmt6 [w0 mu0 v0 w1 mu1 v1])) "\n"))
          (.append sb (str (str/join " " (map (fn [a b] (if (> b a) 1 0)) g0 g1)) "\n"))
          (recur p2 (dec tcases) sb))))))

(-main)
