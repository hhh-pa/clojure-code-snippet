;; gradient-descent: fixed-step gradient descent on x'Ax + b'x.
;; Input: n, matrix A, vector b, start x0, learning rate, iterations.
;; Output: final x then objective value, %.6f.
(ns gradient-descent (:require [clojure.string :as str]))
(def ^:private US java.util.Locale/US)
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Double/parseDouble t))
(defn fmt [x] (String/format US "%.6f" (into-array Object [(+ (double x) 0.0)])))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (let [n (long (Double/parseDouble (nth toks 0)))
            vals (mapv parse-token (subvec toks 1))
            A (vec (for [i (range n)] (vec (for [j (range n)] (nth vals (+ (* i n) j))))))
            b (vec (take n (drop (* n n) vals)))
            x0 (vec (take n (drop (+ (* n n) n) vals)))
            lr (nth vals (+ (* n n) (* 2 n)))
            iters (long (nth vals (+ (* n n) (* 2 n) 1)))
            x (loop [t 0 x x0]
                (if (>= t iters) x
                  (let [g (mapv (fn [i] (+ (* 2 (reduce + (map (fn [j] (* (nth (nth A i) j) (nth x j))) (range n)))) (nth b i))) (range n))]
                    (recur (inc t) (mapv - x (mapv #(* lr %) g))))))
            f (+ (reduce + (for [i (range n) j (range n)] (* (nth x i) (nth (nth A i) j) (nth x j))))
                 (reduce + (map * b x)))]
        (println (str/join " " (map fmt x)))
        (println (fmt f))))))
(-main)
