(require '[clojure.string :as str])
;; sieve: sieve of Eratosthenes. O(n log log n).
;; Pure Clojure with canonical input parsing.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))

(defn parse-token
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))

(defn -main [input]
  (let [n (Long/parseLong (str/trim input))]
    (if (< n 2) (do (println "0") (println))
      (let [is-prime (boolean-array (inc n) true)]
        (aset is-prime 0 false) (aset is-prime 1 false)
        (doseq [i (range 2 (inc (int (Math/sqrt n))))]
          (when (aget is-prime i) (doseq [j (range (* i i) (inc n) i)] (aset is-prime j false))))
        (let [primes (filter #(aget is-prime %) (range 2 (inc n)))]
          (println (count primes)) (println (str/join " " primes)))))))
(-main (slurp *in*))