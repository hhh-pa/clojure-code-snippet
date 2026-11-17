;; ElGamal public-key encryption over a toy prime group.

(defn modpow [b e m]
  (loop [r (mod 1 m) b (mod b m) e e]
    (if (zero? e) r
        (recur (if (odd? e) (mod (* r b) m) r)
               (mod (* b b) m) (quot e 2)))))

(let [toks (vec (.split (.trim (slurp *in*)) "\\s+")) op (first toks)]
  (if (= op "ENC")
    (let [p (bigint (second toks)) g (bigint (nth toks 2)) y (bigint (nth toks 3))
          m (bigint (nth toks 4)) k (bigint (nth toks 5))
          c1 (modpow g k p)]
      (print (str (long c1) " " (long (mod (* m (modpow y k p)) p)))))
    (let [p (bigint (second toks)) x (bigint (nth toks 2)) c1 (bigint (nth toks 3))
          c2 (bigint (nth toks 4))]
      (print (long (mod (* c2 (modpow c1 (- p 1 x) p)) p))))))
