(ns sieve (:require [clojure.string :as str]))
(defn -main []
  (let [n (Long/parseLong (str/trim (slurp *in*)))]
    (if (< n 2) (do (println "0") (println))
      (let [is-prime (boolean-array (inc n) true)]
        (aset is-prime 0 false) (aset is-prime 1 false)
        (doseq [i (range 2 (inc (int (Math/sqrt n))))]
          (when (aget is-prime i) (doseq [j (range (* i i) (inc n) i)] (aset is-prime j false))))
        (let [primes (filter #(aget is-prime %) (range 2 (inc n)))]
          (println (count primes)) (println (str/join " " primes)))))))
(-main)
