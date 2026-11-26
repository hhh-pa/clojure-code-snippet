;; Square root mod n (n = p^k or p*q): general Tonelli-Shanks,
;; Hensel lifting, CRT combine; -1 for non-residues.
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

(defn tonelli [a0 p]
  (let [a (rem (+ (rem a0 p) p) p)]
    (cond (zero? a) 0
          (not= (modpow a (quot (dec p) 2) p) 1) -1
          (= (rem p 4) 3) (modpow a (quot (inc p) 4) p)
          :else
          (let [[q s] (loop [q (dec p)
                             s 0]
                        (if (pos? (rem q 2)) [q s] (recur (quot q 2) (inc s))))
                z (loop [z 2]
                    (if (= (modpow z (quot (dec p) 2) p) (dec p)) z (recur (inc z))))]
            (loop [c (modpow z q p)
                   r (modpow a (quot (inc q) 2) p)
                   t (modpow a q p)
                   m s]
              (if (= t 1)
                r
                (let [i (loop [t2 (rem (* t t) p)
                               i 1]
                          (if (= t2 1) i (recur (rem (* t2 t2) p) (inc i))))
                      b (modpow c (bit-shift-left 1 (- m i 1)) p)]
                  (recur (rem (* b b) p) (rem (* r b) p) (rem (* t (rem (* b b) p)) p) i))))))))

(defn hensel [a p e r0]
  (loop [i 1
         r r0
         m p]
    (if (>= i e)
      r
      (let [m2 (* m p)
            fr (rem (+ (rem (- (* r r) a) m2) m2) m2)]
        (recur (inc i)
               (rem (+ (rem (- r (* fr (modinv (rem (* 2 r) m2) m2))) m2) m2) m2)
               m2)))))

(defn prime-power-root [a0 p e]
  (let [n (reduce * (repeat e p))
        a (rem (+ (rem a0 n) n) n)]
    (if (zero? (rem a p))
      (cond (zero? a) 0
            :else
            (let [[va t] (loop [va 0
                                t a]
                           (if (pos? (rem t p)) [va t] (recur (inc va) (quot t p))))]
              (if (pos? (rem va 2))
                -1
                (let [sub (prime-power-root t p (- e va))]
                  (if (= sub -1)
                    -1
                    (let [r (rem (* sub (reduce * (repeat (quot va 2) p))) n)]
                      (min r (- n r))))))))
      (let [r0 (tonelli a p)]
        (if (= r0 -1) -1 (let [r (hensel a p e r0)] (min r (- n r))))))))

(defn factorize [n]
  (loop [v n
         d 2
         facs {}]
    (if (> (* d d) v)
      (if (> v 1) (assoc facs v (inc (get facs v 0))) facs)
      (if (zero? (rem v d))
        (recur (quot v d) d (assoc facs d (inc (get facs d 0))))
        (recur v (if (= d 2) 3 (+ d 2)) facs)))))

(defn modsqrt [a n]
  (let [facs (factorize n)]
    (cond (= (count facs) 1)
          (let [[p e] (first facs)] (prime-power-root a p e))
          (and (= (count facs) 2) (every? #(= % 1) (vals facs)))
          (let [[[p _] [q _]] (seq facs)
                rp (tonelli a p)
                rq (tonelli a q)]
            (if (or (= rp -1) (= rq -1))
              -1
              (apply min
                     (for [sp [rp (rem (- p rp) p)]
                           sq [rq (rem (- q rq) q)]]
                       (rem (+ sp (* p (rem (+ (rem (* (- sq sp) (modinv (rem p q) q)) q) q) q)))
                            n)))))
          :else -1)))

(defn -main
  "Read a and n; print the smallest square root mod n or -1."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 2)
      (print (modsqrt (Long/parseLong (nth toks 0)) (Long/parseLong (nth toks 1)))))))

(-main (slurp *in*))
