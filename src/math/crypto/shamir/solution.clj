(require (quote [clojure.string :as str]))
;; Shamir secret sharing: split a secret into n shares, combine any k.

(defn egcd [a b]
  (if (zero? b) [a 1 0]
      (let [[g x1 y1] (egcd b (mod a b))] [g y1 (- x1 (* (quot a b) y1))])))

(defn modinv [a m]
  (mod (second (egcd (mod a m) m)) m))

(defn shamir-eval [coeffs x prime]
  (reduce (fn [y c] (mod (+ (* y x) c) prime)) 0 (reverse coeffs)))

(defn shamir-combine [xs ys prime]
  (reduce + 0
          (map-indexed
           (fn [i xi]
             (let [terms (keep-indexed (fn [j xj] (when (not= i j) xj)) xs)
                   num (reduce (fn [a xj] (mod (* a (- xj)) prime)) 1 terms)
                   den (reduce (fn [a xj] (mod (* a (- xi xj)) prime)) 1 terms)]
               (mod (* (nth ys i) num (modinv den prime)) prime)))
           xs)))

(let [toks (vec (.split (.trim (slurp *in*)) "\\s+")) op (first toks)]
  (if (= op "SPLIT")
    (let [prime (bigint (second toks)) secret (bigint (nth toks 2)) n (Integer/parseInt (nth toks 3))
          t (Integer/parseInt (nth toks 4))
          coeffs (into [secret] (map bigint (take (- t 1) (drop 5 toks))))]
      (print (str/join " " (mapcat (fn [x] [x (long (shamir-eval coeffs x prime))]) (range 1 (inc n))))))
    (let [prime (bigint (second toks))
          vals (mapv bigint (drop 2 toks))
          xs (mapv #(nth vals %) (range 0 (count vals) 2))
          ys (mapv #(nth vals %) (range 1 (count vals) 2))]
      (print (long (mod (shamir-combine xs ys prime) prime))))))
