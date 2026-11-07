;; Primality testing plus full factoring, all in pure code.
;;
;; stdin shape: t then t positive integers, one verdict per line.
;; stdout: PRIME or COMPOSITE with sorted prime factors.
;; Uses fixed base Miller-Rabin verdicts, trial division by small
;; primes, then Brent rho with batched gcd. Rho expected cost is the
;; fourth root of n with far fewer gcd calls than one gcd per step.
;; Blank stdin gives no output. Bad input gives a stderr note and the
;; script returns normally, so exit stays zero with no host exit call.

(require '[clojure.string :as str])

(def digit-values
  "Digit char to int value."
  (zipmap "0123456789" (range)))

(defn parse-int
  "Parse a signed decimal token with exact math."
  [tok]
  (if (and (<= (count tok) 18) (re-matches #"-?[0-9]+" tok))
    (parse-long tok)
    (let [neg (= \- (first tok))
          body (if neg (rest tok) (seq tok))]
      (let [v (reduce (fn [acc ch] (+' (*' acc 10) (digit-values ch)))
                      0
                      body)]
        (if neg (-' v) v)))))

(defn int-token?
  "True when tok is an optionally signed run of decimal digits."
  [tok]
  (boolean (and (string? tok) (re-matches #"-?[0-9]+" tok))))

(defn fail
  "Print message to stderr, then return normally with exit zero."
  [msg]
  (binding [*out* *err*]
    (println msg)))

(def mr-bases
  "Fixed Miller-Rabin bases, same verdicts as the old script."
  [2 3 5 7 11 13 17 19 23 29 31 37])

(def small-primes
  "Trial division primes, cheap constant work before rho starts."
  [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97])

(defn mod-mul
  "Modular multiply that stays exact for big values."
  [a b m]
  (if (and (int? m) (< m 3037000499))
    (rem (* (long a) (long b)) (long m))
    (mod (*' a b) m)))

(defn mod-pow
  "Modular power by squaring."
  [base exp m]
  (if (and (int? m) (< m 3037000499) (int? exp))
    (let [ml (long m)]
      (loop [res 1
             b (rem (long base) ml)
             e (long exp)]
        (if (zero? e)
          res
          (recur (if (odd? e) (rem (* res b) ml) res)
                 (rem (* b b) ml)
                 (bit-shift-right e 1)))))
    (loop [res 1
           b (mod base m)
           e exp]
      (if (zero? e)
        res
        (recur (if (odd? e) (mod (*' res b) m) res)
               (mod (*' b b) m)
               (quot e 2))))))

(defn gcd
  "Euclid gcd over exact integers."
  [a b]
  (loop [x a
         y b]
    (if (zero? y)
      x
      (recur y (mod x y)))))

(defn abs-diff
  "Exact absolute difference of two integers."
  [u v]
  (if (> u v) (-' u v) (-' v u)))

(defn miller-rabin?
  "Fixed base Miller-Rabin verdicts matching the old script."
  [n]
  (cond
    (< n 2) false
    (or (== n 2) (== n 3)) true
    (even? n) false
    :else
    (let [[d s] (loop [d (dec n)
                       s 0]
                  (if (even? d)
                    (recur (quot d 2) (inc s))
                    [d s]))
          nm1 (dec n)
          nb (count mr-bases)]
      (loop [idx 0]
        (if (>= idx nb)
          true
          (let [a (nth mr-bases idx)]
            (if (<= n a)
              true
              (let [x (mod-pow a d n)]
                (if (or (== x 1) (== x nm1))
                  (recur (inc idx))
                  (if (loop [r 1
                             y x]
                        (if (>= r s)
                          false
                          (let [y2 (mod-mul y y n)]
                            (if (== y2 nm1)
                              true
                              (recur (inc r) y2)))))
                    (recur (inc idx))
                    false))))))))))

(defn brent-with-c
  "One Brent rho attempt with step fn f. Gives a factor of n."
  [n f batch]
  (loop [y 2
         r 1
         q 1
         g 1
         x 0
         ys 0]
    (cond
      (and (not (== g 1)) (not (== g n)))
      g

      (== g n)
      (loop [w (f ys)]
        (let [gg (gcd (abs-diff x w) n)]
          (if (== gg 1)
            (recur (f w))
            gg)))

      :else
      (let [x-new y
            y-new (loop [i 0
                         v y]
                    (if (>= i r)
                      v
                      (recur (inc i) (f v))))
            [y-end q-end g-end ys-end]
            (loop [k 0
                   v y-new
                   qq q
                   gg g
                   ws ys]
              (if (or (>= k r) (not (== gg 1)))
                [v qq gg ws]
                (let [lim (min batch (- r k))
                      [v2 qq2] (loop [i 0
                                      vv v
                                      pp qq]
                                 (if (>= i lim)
                                   [vv pp]
                                   (let [nn (f vv)]
                                     (recur (inc i)
                                            nn
                                            (mod-mul pp (abs-diff x-new nn) n)))))]
                  (recur (+ k batch) v2 qq2 (gcd qq2 n) v))))]
        (recur y-end (* r 2) q-end g-end x-new ys-end)))))

(defn pollard-rho
  "Nontrivial factor of odd composite n, retrying fresh addends."
  [n]
  (if (even? n)
    2
    (loop [c 1]
      (let [f (fn [v] (mod (+' (mod-mul v v n) c) n))
            d (brent-with-c n f 128)]
        (if (and (> d 1) (< d n))
          d
          (recur (inc c)))))))

(defn split-off-small
  "Trial divide by small primes. Gives [small factors rest]."
  [n]
  (let [np (count small-primes)]
    (loop [idx 0
           left n
           found []]
      (if (>= idx np)
        [found left]
        (let [p (nth small-primes idx)]
          (if (zero? (mod left p))
            (recur idx (quot left p) (conj found p))
            (recur (inc idx) left found)))))))

(defn factorize
  "Full prime factor list of n, unsorted, exact integers."
  [n]
  (cond
    (== n 1) []
    (miller-rabin? n) [n]
    :else
    (let [[smalls left] (split-off-small n)]
      (cond
        (== left 1) (vec smalls)
        (miller-rabin? left) (conj (vec smalls) left)
        :else
        (let [d (pollard-rho left)]
          (into (vec smalls)
                (into (factorize d)
                      (factorize (quot left d)))))))))

(defn -main
  "Read stdin, print one PRIME or COMPOSITE line per value."
  []
  (let [input (slurp *in*)]
    (when-not (str/blank? input)
      (let [toks (vec (str/split (str/trim input) #"\s+"))]
        (cond
          (not (int-token? (first toks)))
          (fail "Error: first token must be integer t")

          :else
          (let [t (parse-int (first toks))
                rest-toks (subvec toks 1)]
            (cond
              (neg? t)
              (fail "Error: t must be non-negative")

              (< (count rest-toks) t)
              (fail "Error: expected t values after t")

              (let [slice (subvec rest-toks 0 t)]
                (not (every? int-token? slice)))
              (fail "Error: values must be integers")

              :else
              (let [nums (mapv parse-int (subvec rest-toks 0 t))]
                (if (some (fn [v] (<= v 0)) nums)
                  (fail "Error: values must be positive integers")
                  (doseq [n nums]
                    (if (miller-rabin? n)
                      (println "PRIME")
                      (let [factors (sort (factorize n))]
                        (println (str "COMPOSITE "
                                      (str/join " " factors)))))))))))))))

(-main)
