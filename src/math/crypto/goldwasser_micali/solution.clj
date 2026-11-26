;; Goldwasser-Micali bit-by-bit probabilistic encryption (toy modulus).
(require '[clojure.string :as str])

(defn modpow [b e m]
  (loop [r (mod 1 m) b (mod b m) e e]
    (if (zero? e) r
        (recur (if (odd? e) (mod (* r b) m) r)
               (mod (* b b) m) (quot e 2)))))

(let [toks (vec (.split (.trim (slurp *in*)) "\\s+")) op (first toks)]
  (if (= op "ENC")
    (let [n (bigint (second toks)) x (bigint (nth toks 2)) bits (nth toks 3)
          rs (map bigint (drop 4 toks))]
      (print (str/join " "
               (map (fn [b r] (long (mod (* (* r r) (if (= b \1) x 1)) n)))
                    bits rs))))
    (let [p (bigint (second toks)) cs (map bigint (drop 3 toks)) e (quot (dec p) 2)]
      (print (apply str (map (fn [c] (if (= (modpow c e p) (dec p)) "1" "0")) cs))))))
