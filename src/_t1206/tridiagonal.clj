;; tridiagonal: Thomas O(n) algorithm for tridiagonal linear systems.
;; Input: n, then lower(n-1) diagonal(n) upper(n-1) rhs(n) values
;; (for n=1: just diagonal and rhs). Output: solution with %.6f
;; entries, or SINGULAR.
(ns tridiagonal (:require [clojure.string :as str]))
(def ^:const EPS 1e-12)
(def ^:private US java.util.Locale/US)
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Double/parseDouble t))
(defn fmt [x] (String/format US "%.6f" (into-array Object [(double x)])))
(defn thomas [lo diag hi rhs]
  (let [n (count diag)]
    (if (= n 1)
      (when (>= (Math/abs (nth diag 0)) EPS)
        [(/ (nth rhs 0) (nth diag 0))])
      (when (>= (Math/abs (nth diag 0)) EPS)
        (loop [i 1
               cp (assoc (vec (repeat (dec n) 0.0)) 0 (/ (nth hi 0) (nth diag 0)))
               dp (assoc (vec (repeat n 0.0)) 0 (/ (nth rhs 0) (nth diag 0)))]
          (if (< i (dec n))
            (let [denom (- (nth diag i) (* (nth lo (dec i)) (nth cp (dec i))))]
              (if (< (Math/abs denom) EPS) nil
                (recur (inc i)
                       (assoc cp i (/ (nth hi i) denom))
                       (assoc dp i (/ (- (nth rhs i) (* (nth lo (dec i)) (nth dp (dec i)))) denom)))))
            (let [denom (- (nth diag (dec n)) (* (nth lo (- n 2)) (nth cp (- n 2))))]
              (when (>= (Math/abs denom) EPS)
                (let [dp (assoc dp (dec n)
                                (/ (- (nth rhs (dec n)) (* (nth lo (- n 2)) (nth dp (- n 2)))) denom))]
                  (loop [i (- n 2) x (assoc (vec (repeat n 0.0)) (dec n) (nth dp (dec n)))]
                    (if (neg? i) x
                      (recur (dec i) (assoc x i (- (nth dp i) (* (nth cp i) (nth x (inc i)))))))))))))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (let [n (long (Double/parseDouble (nth toks 0)))
            vals (mapv parse-token (subvec toks 1))]
        (when (pos? n)
          (let [[lo diag hi rhs] (if (= n 1)
                                   [[] (subvec vals 0 1) [] (subvec vals 1 2)]
                                   [(subvec vals 0 (dec n))
                                    (subvec vals (dec n) (+ (dec n) n))
                                    (subvec vals (+ (dec n) n) (+ (* 2 (dec n)) n))
                                    (subvec vals (+ (* 2 (dec n)) n) (+ (* 2 (dec n)) (* 2 n)))])
                sol (thomas lo diag hi rhs)]
            (if (nil? sol)
              (println "SINGULAR")
              (println (str/join " " (map fmt sol))))))))))
(-main)
