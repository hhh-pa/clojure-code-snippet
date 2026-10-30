(ns pollard-pm1 (:require [clojure.string :as str]))
(defn modpow [b e m]
  (loop [r (mod 1 m) b (mod b m) e e] (if (zero? e) r (recur (if (odd? e) (mod (* r b) m) r) (mod (* b b) m) (quot e 2)))))
(defn gcd [a b] (if (zero? b) (Math/abs a) (recur b (mod a b))))
(defn -main []
  (let [toks (str/split (str/trim (slurp *in*)) #"\s+")
        n (Long/parseLong (nth toks 0)) B (Long/parseLong (nth toks 1))]
    (if (<= n 1) (println "NONE")
      (loop [a 2 k 2]
        (if (> k B)
          (let [g (gcd (dec a) n)] (if (and (> g 1) (< g n)) (println g) (println "NONE")))
          (let [a2 (modpow a k n) g (gcd (dec a2) n)]
            (if (and (> g 1) (< g n)) (println g) (recur a2 (inc k)))))))))
(-main)
