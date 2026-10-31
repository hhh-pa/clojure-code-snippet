;; solovay-strassen: Jacobi symbol and Solovay-Strassen primality test.
;; Input: n t then t bases. Output: Jacobi values, then PRIME/COMPOSITE.
(ns solovay-strassen (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Long/parseLong t))
(defn llgcd [a b]
  (loop [a (Math/abs (long a)) b (Math/abs (long b))]
    (if (zero? b) a (recur b (mod a b)))))
(defn modpow [b e m]
  (loop [r (mod 1 m) b (mod b m) e e]
    (if (zero? e) r
      (recur (if (odd? e) (mod (* r b) m) r) (mod (* b b) m) (quot e 2)))))
(defn jacobi [a0 n0]
  (loop [a (mod a0 n0) n n0 t 1]
    (if (zero? a) (if (= n 1) t 0)
      (let [[a t] (loop [a a t t]
                    (if (odd? a) [a t]
                      (let [r (mod n 8)]
                        (recur (quot a 2) (if (or (= r 3) (= r 5)) (- t) t)))))
            a2 n n2 a
            t2 (if (and (= 3 (mod a2 4)) (= 3 (mod n2 4))) (- t) t)]
        (recur (mod a2 n2) n2 t2)))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 2)
      (let [vals (mapv parse-token toks)
            n (nth vals 0) nt (nth vals 1)
            b (vec (take nt (drop 2 vals)))
            res (cond (< n 2) [(vec (repeat nt 0)) true]
                      (= n 2) [(mapv #(jacobi % n) b) false]
                      (even? n) [(vec (repeat nt 0)) true]
                      :else (loop [i 0 js [] comp false]
                              (if (>= i nt) [js comp]
                                (let [j (jacobi (nth b i) n)
                                      bad (or (> (llgcd (nth b i) n) 1)
                                              (zero? j)
                                              (not= (modpow (nth b i) (quot (dec n) 2) n) (mod j n)))]
                                  (recur (inc i) (conj js j) (or comp bad))))))]
        (println (str/join " " (map str (nth res 0))))
        (println (if (nth res 1) "COMPOSITE" "PRIME"))))))
(-main)
