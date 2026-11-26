;; Pohlig-Hellman discrete log: trial-factor p-1, brute-force DLOG
;; per prime-order subgroup with q^e digit lifting, CRT combine.
(require '[clojure.string :as str])

(defn modpow [b e m]
  (loop [r (rem 1 m)
         b (rem b m)
         e e]
    (if (zero? e)
      r
      (recur (if (pos? (bit-and e 1)) (rem (* r b) m) r)
             (rem (* b b) m)
             (bit-shift-right e 1)))))

(defn egcd [a b]
  (if (zero? b)
    [a 1 0]
    (let [[g x1 y1] (egcd b (rem a b))]
      [g y1 (- x1 (* (quot a b) y1))])))

(defn modinv [a m]
  (rem (+ (rem (second (egcd a m)) m) m) m))

(defn factorize [n]
  (loop [v n
         d 2
         facs {}]
    (if (> (* d d) v)
      (if (> v 1) (assoc facs v (inc (get facs v 0))) facs)
      (if (zero? (rem v d))
        (recur (quot v d) d (assoc facs d (inc (get facs d 0))))
        (recur v (if (= d 2) 3 (+ d 2)) facs)))))

(defn subgroup-log [g h p q e n]
  (let [qe (reduce * (repeat e q))
        gi (modpow g (quot n qe) p)
        hi (modpow h (quot n qe) p)
        gamma (modpow gi (reduce * (repeat (dec e) q)) p)
        table (loop [j 0
                     cur 1
                     t {}]
                (if (>= j q)
                  t
                  (recur (inc j) (rem (* cur gamma) p) (assoc t cur j))))]
    (loop [k 0
           xi 0
           hcur hi
           qk 1]
      (if (>= k e)
        xi
        (let [dk (get table (modpow hcur (reduce * (repeat (- e 1 k) q)) p))
              sub (rem (* dk qk) (dec p))]
          (recur (inc k)
                 (+ xi (* dk qk))
                 (rem (* hcur (modinv (modpow gi sub p) p)) p)
                 (* qk q)))))))

(defn pohlig [p g h]
  (let [n (dec p)
        facs (factorize n)
        [x _] (reduce (fn [[x m] [q e]]
                        (let [qe (reduce * (repeat e q))
                              xi (subgroup-log g h p q e n)
                              inv (modinv (rem m qe) qe)
                              t (rem (+ (rem (* (- xi x) inv) qe) qe) qe)]
                          [(rem (+ x (* m t)) (* m qe)) (* m qe)]))
                      [0 1]
                      facs)]
    (rem (+ (rem x n) n) n)))

(defn -main
  "Read p, g, h; print the discrete log of h base g."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 3)
      (print (pohlig (Long/parseLong (nth toks 0))
                     (Long/parseLong (nth toks 1))
                     (Long/parseLong (nth toks 2)))))))

(-main (slurp *in*))
