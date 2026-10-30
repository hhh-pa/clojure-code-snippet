;; lucas-lehmer: Lucas-Lehmer test for Mersenne primes M_p = 2^p - 1.
;; Input: exponents, one verdict (PRIME/COMPOSITE) per line.
(ns lucas-lehmer (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Long/parseLong t))
(defn ll-prime? [p]
  (if (= p 2) true
    (let [M (dec (bit-shift-left 1 p))]
      (zero? (loop [i 0 s 4]
               (if (>= i (- p 2)) s
                 (recur (inc i) (mod (- (* s s) 2) M))))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (doseq [t toks]
      (println (if (ll-prime? (parse-token t)) "PRIME" "COMPOSITE")))))
(-main)
