;; Paillier additive homomorphic encryption (toy modulus).

(defn modpow [b e m]
  (loop [r (mod 1 m) b (mod b m) e e]
    (if (zero? e) r
        (recur (if (odd? e) (mod (* r b) m) r)
               (mod (* b b) m) (quot e 2)))))

(let [toks (vec (.split (.trim (slurp *in*)) "\\s+")) op (first toks)]
  (cond
    (= op "ENC")
    (let [n (bigint (second toks)) g (bigint (nth toks 2)) m (bigint (nth toks 3))
          r (bigint (nth toks 4)) n2 (* n n)]
      (print (long (mod (* (modpow g m n2) (modpow r n n2)) n2))))
    (= op "DEC")
    (let [n (bigint (second toks)) lam (bigint (nth toks 2)) mu (bigint (nth toks 3))
          c (bigint (nth toks 4)) n2 (* n n)]
      (print (long (mod (* (quot (dec (modpow c lam n2)) n) mu) n))))
    :else
    (let [n (bigint (second toks)) c1 (bigint (nth toks 2)) c2 (bigint (nth toks 3))]
      (print (long (mod (* c1 c2) (* n n)))))))
