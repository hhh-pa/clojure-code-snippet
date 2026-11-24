;; Non-uniform deviates (NR 7.3).
(require '[clojure.string :as str])

(defn fmt6 [v]
  (let [neg (neg? v)
        x (if neg (- v) v)
        ip (long x)
        fp (long (+ (* (- x ip) 1000000.0) 0.5))
        [ip fp] (if (= fp 1000000) [(inc ip) 0] [ip fp])]
    (str (when neg "-") ip "." (subs (str (+ 1000000 fp)) 1))))

(defn absv [v] (if (neg? v) (- v) v))

(defn canon [v] (if (< (absv v) 5e-7) 0.0 v))

(defn u+ [a b] (unchecked-add a b))
(defn u* [a b] (unchecked-multiply a b))

(def VINIT 4101842887655102017)
(def DOUBC 5.42101086242752217E-20)

(defn parse-u64 [s]
  (let [cs (seq s)
        cs (if (= (first cs) \+) (next cs) cs)]
    (reduce (fn [acc ch] (u+ (u* acc 10) (Character/digit ^char ch 10))) 0 cs)))

(defn u64-dbl [v]
  (+ (* 4294967296.0 (double (unsigned-bit-shift-right v 32)))
     (double (bit-and v 0xFFFFFFFF))))

(defn ran-step [[u v w]]
  (let [u (u+ (u* u 2862933555777941757) 7046029254386353087)
        v (bit-xor v (unsigned-bit-shift-right v 17))
        v (bit-xor v (bit-shift-left v 31))
        v (bit-xor v (unsigned-bit-shift-right v 8))
        w (u+ (u* 4294957665 (bit-and w 0xFFFFFFFF)) (unsigned-bit-shift-right w 32))
        x (bit-xor u (bit-shift-left u 21))
        x (bit-xor x (unsigned-bit-shift-right x 35))
        x (bit-xor x (bit-shift-left x 4))]
    [u v w (bit-xor (u+ x v) w)]))

(defn ran-init [j]
  (let [[u1 _ w1 _] (ran-step [(bit-xor j VINIT) VINIT 1])
        [u2 v2 _ _] (ran-step [u1 u1 w1])
        [u3 v3 w3 _] (ran-step [u2 v2 v2])]
    [u3 v3 w3]))

(defn ran-doub [st]
  (let [[u v w o] (ran-step st)]
    [[u v w] (* DOUBC (u64-dbl o))]))

(defn ran-int64 [st]
  (let [[u v w o] (ran-step st)]
    [[u v w] o]))

(def gcof [57.1562356658629235 -59.5979603554754912 14.1360979747417471
           -0.491913816097620199 0.339946499848118887e-4 0.465236289270485756e-4
           -0.983744753048795646e-4 0.158088703224912494e-3 -0.210264441724104883e-3
           0.217439618115212643e-3 -0.164318106536763890e-3 0.844182239838527433e-4
           -0.261908384015814087e-4 0.368991826595316234e-5])

(defn gammln [xx]
  (let [x xx
        tmp (+ x 5.2421875000000000)
        tmp (- (* (+ x 0.5) (Math/log tmp)) tmp)]
    (loop [j 0 y xx ser 0.999999999999997092]
      (if (>= j 14)
        (+ tmp (Math/log (/ (* 2.5066282746310005 ser) x)))
        (let [y (+ y 1.0)] (recur (inc j) y (+ ser (/ (nth gcof j) y))))))))

(defn expon-dev [st bet]
  (loop [st st]
    (let [[st u] (ran-doub st)]
      (if (not= u 0.0)
        [st (/ (- (Math/log u)) bet)]
        (recur st)))))

(defn logistic-dev [st mu sig]
  (loop [st st]
    (let [[st u] (ran-doub st)]
      (if (not= (* u (- 1.0 u)) 0.0)
        [st (+ mu (* (* 0.551328895421792050 sig) (Math/log (/ u (- 1.0 u)))))]
        (recur st)))))

(defn cauchy-dev [st mu sig]
  (loop [st st]
    (let [[st v1] (ran-doub st)
          v1 (- (* 2.0 v1) 1.0)
          [st v2] (ran-doub st)]
      (if (not (or (>= (+ (* v1 v1) (* v2 v2)) 1.0) (= v2 0.0)))
        [st (+ mu (/ (* sig v1) v2))]
        (recur st)))))

(defn normal-dev [st mu sig]
  (loop [st st]
    (let [[st u] (ran-doub st)
          [st vd] (ran-doub st)
          v (* 1.7156 (- vd 0.5))
          x (- u 0.449871)
          y (+ (absv v) 0.386595)
          q (+ (* x x) (* y (- (* 0.19600 y) (* 0.25472 x))))]
      (if (not (and (> q 0.27597) (or (> q 0.27846) (> (* v v) (* (* -4.0 (Math/log u)) (* u u))))))
        [st (+ mu (/ (* sig v) u))]
        (recur st)))))

(defn boxmuller-dev [st mu sig stored]
  (if (= stored 0.0)
    (loop [st st]
      (let [[st v1] (ran-doub st)
            v1 (- (* 2.0 v1) 1.0)
            [st v2] (ran-doub st)
            v2 (- (* 2.0 v2) 1.0)
            rsq (+ (* v1 v1) (* v2 v2))]
        (if (not (or (>= rsq 1.0) (= rsq 0.0)))
          (let [fac (Math/sqrt (/ (* -2.0 (Math/log rsq)) rsq))]
            [st (* v1 fac) (+ mu (* sig (* v2 fac)))])
          (recur st))))
    [st 0.0 (+ mu (* sig stored))]))

(defn gamma-dev [st alph bet]
  (let [oalph alph
        alph (if (< alph 1.0) (+ alph 1.0) alph)
        a1 (- alph (/ 1.0 3.0))
        a2 (/ 1.0 (Math/sqrt (* 9.0 a1)))]
    (loop [st st]
      (let [[st x v] (loop [st st]
                       (let [[st x] (normal-dev st 0.0 1.0)
                             v (+ 1.0 (* a2 x))]
                         (if (> v 0.0) [st x v] (recur st))))
            v (* (* v v) v)
            [st u] (ran-doub st)]
        (if (not (and (> u (- 1.0 (* 0.331 (* (* x x) (* x x)))))
                      (> (Math/log u) (+ (* 0.5 (* x x)) (* a1 (+ (- 1.0 v) (Math/log v)))))))
          (if (= alph oalph)
            [st (/ (* a1 v) bet)]
            (loop [st st]
              (let [[st u] (ran-doub st)]
                (if (not= u 0.0)
                  [st (/ (* (* (Math/pow u (/ 1.0 oalph)) a1) v) bet)]
                  (recur st)))))
          (recur st))))))

(defn poisson-dev [st lam]
  (if (< lam 5.0)
    (let [lamexp (Math/exp (- lam))]
      (loop [st st k -1 t 1.0]
        (let [k (inc k)
              [st d] (ran-doub st)
              t (* t d)]
          (if (> t lamexp) (recur st k t) [st k]))))
    (let [sqlam (Math/sqrt lam) loglam (Math/log lam)]
      (loop [st st]
        (let [[st u0] (ran-doub st)
              u (* 0.64 u0)
              [st v0] (ran-doub st)
              v (+ -0.68 (* 1.28 v0))]
          (if (= u 0.0)
            (recur st)
            (let [v2 (* v v)]
              (if (and (> lam 13.5)
                       (if (>= v 0.0)
                         (> v2 (* (* 6.5 u) (* (- 0.64 u) (+ u 0.2))))
                         (> v2 (* (* 9.6 u) (* (- 0.66 u) (+ u 0.07))))))
                (recur st)
                (let [kf (Math/floor (+ (* sqlam (/ v u)) lam 0.5))]
                  (if (or (< kf 0.0) (>= kf 9.223372036854776e18))
                    (recur st)
                    (let [k (long kf) u2 (* u u)]
                      (if (and (> lam 13.5)
                               (if (>= v 0.0)
                                 (< v2 (* (* 15.2 u2) (* (- 0.61 u) (- 0.8 u))))
                                 (< v2 (* (* 6.76 u2) (* (- 0.62 u) (- 1.4 u))))))
                        [st k]
                        (let [lfac (gammln (+ (double k) 1.0))
                              p (* sqlam (Math/exp (+ (- lam) (* (double k) loglam) (- lfac))))]
                          (if (< u2 p) [st k] (recur st)))))))))))))))

(defn binomial-prep [nn pp]
  (let [p (if (<= pp 0.5) pp (- 1.0 pp))]
    (cond
      (<= nn 64) (loop [j 0 pb p bits []]
                   (if (>= j 5)
                     {:swch 0 :p p :pp pp :n nn :pb (- pb (Math/floor pb)) :pbits (vec bits)}
                     (let [pb (* pb 2.0)]
                       (recur (inc j) pb (conj bits (bit-and 1 (long pb)))))))
      (< (* (double nn) p) 30.0)
      (let [c0 (Math/exp (* (double nn) (Math/log (- 1.0 p))))]
        (loop [j 1 cdf [c0]]
          (if (>= j 64)
            {:swch 1 :p p :pp pp :n nn :cdf (vec cdf)}
            (recur (inc j) (conj cdf (+ (nth cdf (dec j))
                                         (Math/exp (+ (- (gammln (+ (double nn) 1.0)) (gammln (+ (double j) 1.0)))
                                                       (- (gammln (+ (- (double nn) (double j)) 1.0)))
                                                       (* (double j) (Math/log p))
                                                       (* (- (double nn) (double j)) (Math/log (- 1.0 p)))))))))))
      :else {:swch 2 :p p :pp pp :n nn :np (* (double nn) p) :glnp (gammln (+ (double nn) 1.0))
             :plog (Math/log p) :pclog (Math/log (- 1.0 p))
             :sq (Math/sqrt (* (* (double nn) p) (- 1.0 p)))
             :logfact (if (< nn 1024) (vec (map #(gammln (+ (double %) 1.0)) (range (inc nn)))) nil)})))

(defn binomial-dev [st prep]
  (let [swch (:swch prep)]
    (cond
      (= swch 0)
      (let [uo -1
            [st unfin rltp] (loop [j 0 st st unfin uo rltp 0]
                              (if (>= j 5)
                                [st unfin rltp]
                                (let [[st r] (ran-int64 st)
                                      sel (if (not= (nth (:pbits prep) j) 0) uo 0)
                                      diff (bit-and unfin (bit-xor r sel))
                                      rltp (if (not= (nth (:pbits prep) j) 0)
                                             (bit-or rltp diff)
                                             (bit-and rltp (bit-not diff)))
                                      unfin (bit-and unfin (bit-not diff))]
                                  (recur (inc j) st unfin rltp))))
            n (:n prep)]
        (loop [j 0 st st unfin unfin rltp rltp k 0]
          (if (>= j n)
            [st (if (not= (:p prep) (:pp prep)) (- n k) k)]
            (let [[st k] (if (not= (bit-and unfin 1) 0)
                           (let [[st d] (ran-doub st)]
                             [st (if (< d (:pb prep)) (inc k) k)])
                           [st (if (not= (bit-and rltp 1) 0) (inc k) k)])]
              (recur (inc j) st (unsigned-bit-shift-right unfin 1)
                     (unsigned-bit-shift-right rltp 1) k)))))
      (= swch 1)
      (let [[st y] (ran-doub st)]
        (loop [kl -1 k 64]
          (if (<= (- k kl) 1)
            [st (if (not= (:p prep) (:pp prep)) (- (:n prep) k) k)]
            (let [km (quot (+ kl k) 2)]
              (if (< y (nth (:cdf prep) km)) (recur kl km) (recur km k))))))
      :else
      (let [n (:n prep)]
        (loop [st st]
          (let [[st u0] (ran-doub st)
                u (* 0.645 u0)
                [st v0] (ran-doub st)
                v (+ -0.63 (* 1.25 v0))
                v2 (* v v)]
            (if (if (>= v 0.0)
                   (> v2 (* (* 6.5 u) (* (- 0.645 u) (+ u 0.2))))
                   (> v2 (* (* 8.4 u) (* (- 0.645 u) (+ u 0.1)))))
              (recur st)
              (let [kf (Math/floor (+ (* (:sq prep) (/ v u)) (:np prep) 0.5))]
                (if (or (< kf 0.0) (> kf (double n)) (and (not (< kf 0.0)) (not (>= kf 0.0))))
                  (recur st)
                  (let [k (long kf) u2 (* u u)]
                    (if (if (>= v 0.0)
                          (< v2 (* (* 12.25 u2) (* (- 0.615 u) (- 0.92 u))))
                          (< v2 (* (* 7.84 u2) (* (- 0.615 u) (- 1.2 u)))))
                      [st (if (not= (:p prep) (:pp prep)) (- n k) k)]
                      (let [lf (if (< n 1024)
                                 (+ (nth (:logfact prep) k) (nth (:logfact prep) (- n k)))
                                 (+ (gammln (+ (double k) 1.0)) (gammln (+ (- (double n) (double k)) 1.0))))
                            b (* (:sq prep)
                                 (Math/exp (- (+ (:glnp prep) (* (double k) (:plog prep))
                                                  (* (- (double n) (double k)) (:pclog prep)))
                                               lf)))]
                        (if (< u2 b)
                          [st (if (not= (:p prep) (:pp prep)) (- n k) k)]
                          (recur st))))))))))))))

(defn emit-floats [dev-fn st n]
  (loop [i 0 st st acc []]
    (if (>= i n)
      acc
      (let [[st v] (dev-fn st)]
        (recur (inc i) st (conj acc (fmt6 (canon v))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          f (fn [i] (Double/parseDouble (nth toks i)))
          pu (fn [i] (parse-u64 (nth toks i)))
          pi (fn [i] (Long/parseLong (nth toks i)))]
      (println
       (str/join " "
        (cond
          (= op "EXP") (emit-floats (fn [st] (expon-dev st (f 1))) (ran-init (pu 2)) (pi 3))
          (= op "LOGISTIC") (emit-floats (fn [st] (logistic-dev st (f 1) (f 2))) (ran-init (pu 3)) (pi 4))
          (= op "CAUCHY") (emit-floats (fn [st] (cauchy-dev st (f 1) (f 2))) (ran-init (pu 3)) (pi 4))
          (= op "NORMAL") (emit-floats (fn [st] (normal-dev st (f 1) (f 2))) (ran-init (pu 3)) (pi 4))
          (= op "BOXMULLER")
          (let [n (pi 4)]
            (loop [i 0 st (ran-init (pu 3)) sv 0.0 acc []]
              (if (>= i n)
                acc
                (let [[st sv v] (boxmuller-dev st (f 1) (f 2) sv)]
                  (recur (inc i) st sv (conj acc (fmt6 (canon v))))))))
          (= op "RAYLEIGH")
          (let [n (pi 2)]
            (loop [i 0 st (ran-init (pu 1)) acc []]
              (if (>= i n)
                acc
                (let [[st x] (loop [st st]
                               (let [[st x] (ran-doub st)]
                                 (if (not= x 0.0) [st x] (recur st))))]
                  (recur (inc i) st (conj acc (fmt6 (canon (Math/sqrt (* -2.0 (Math/log x)))))))))))
          (= op "GAMMA") (emit-floats (fn [st] (gamma-dev st (f 1) (f 2))) (ran-init (pu 3)) (pi 4))
          (= op "CHISQ_DEV")
          (emit-floats (fn [st] (let [[st v] (gamma-dev st (* 0.5 (f 1)) 1.0)] [st (* 2.0 v)]))
                       (ran-init (pu 2)) (pi 3))
          (= op "STUDENTT_DEV")
          (let [n (pi 5) nu (f 1) mu (f 2) sig (f 3)]
            (loop [i 0 st (ran-init (pu 4)) acc []]
              (if (>= i n)
                acc
                (let [[st u1] (loop [st st]
                                (let [[st u] (ran-doub st)]
                                  (if (not= u 0.0) [st u] (recur st))))
                      [st u2] (ran-doub st)
                      y (* (Math/sqrt (- (Math/pow u1 (/ -2.0 nu)) 1.0))
                            (Math/cos (* 6.283185307179586 u2)))]
                  (recur (inc i) st (conj acc (fmt6 (canon (+ mu (* sig y))))))))))
          (= op "BETA_DEV")
          (let [n (pi 4)]
            (loop [i 0 sx (ran-init (pu 3)) sy (ran-init (unchecked-inc (pu 3))) acc []]
              (if (>= i n)
                acc
                (let [[sx x] (gamma-dev sx (f 1) 1.0)
                      [sy y] (gamma-dev sy (f 2) 1.0)]
                  (recur (inc i) sx sy (conj acc (fmt6 (canon (/ x (+ x y))))))))))
          (= op "F_DEV")
          (let [n (pi 4) nu1 (f 1) nu2 (f 2)]
            (loop [i 0 sx (ran-init (pu 3)) sy (ran-init (unchecked-inc (pu 3))) acc []]
              (if (>= i n)
                acc
                (let [[sx x] (gamma-dev sx (* 0.5 nu1) 1.0)
                      [sy y] (gamma-dev sy (* 0.5 nu2) 1.0)
                      b (/ x (+ x y))]
                  (recur (inc i) sx sy (conj acc (fmt6 (canon (/ (* nu2 b) (* nu1 (- 1.0 b)))))))))))
          (= op "POISSON")
          (let [n (pi 3)]
            (loop [i 0 st (ran-init (pu 2)) acc []]
              (if (>= i n)
                acc
                (let [[st k] (poisson-dev st (f 1))]
                  (recur (inc i) st (conj acc (str k)))))))
          (= op "BINOMIAL")
          (let [n (pi 4) prep (binomial-prep (pi 1) (f 2))]
            (loop [i 0 st (ran-init (pu 3)) acc []]
              (if (>= i n)
                acc
                (let [[st k] (binomial-dev st prep)]
                  (recur (inc i) st (conj acc (str k)))))))))))))

