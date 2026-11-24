;; Statistical distributions (NR 6.14).
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

(def geps 2.220446049250313e-16)
(def gfpmin 1.0020841800044864e-292)

(def gcof [57.1562356658629235 -59.5979603554754912 14.1360979747417471
           -0.491913816097620199 0.339946499848118887e-4 0.465236289270485756e-4
           -0.983744753048795646e-4 0.158088703224912494e-3 -0.210264441724104883e-3
           0.217439618115212643e-3 -0.164318106536763890e-3 0.844182239838527433e-4
           -0.261908384015814087e-4 0.368991826595316234e-5])

(def erfcof [-1.3026537197817094 6.4196979235649026e-1 1.9476473204185836e-2
             -9.561514786808631e-3 -9.46595344482036e-4 3.66839497852761e-4
             4.2523324806907e-5 -2.0278578112534e-5 -1.624290004647e-6
             1.303655835580e-6 1.5626441722e-8 -8.5238095915e-8
             6.529054439e-9 5.059343495e-9 -9.91364156e-10
             -2.27365122e-10 9.6467911e-11 2.394038e-12
             -6.886027e-12 8.94487e-13 3.13092e-13
             -1.12708e-13 3.81e-16 7.106e-15
             -1.523e-15 -9.4e-17 1.21e-16 -2.8e-17])

(defn gammln [xx]
  (let [x xx
        tmp (+ x 5.2421875000000000)
        tmp (- (* (+ x 0.5) (Math/log tmp)) tmp)]
    (loop [j 0 y xx ser 0.999999999999997092]
      (if (>= j 14)
        (+ tmp (Math/log (/ (* 2.5066282746310005 ser) x)))
        (let [y (+ y 1.0)] (recur (inc j) y (+ ser (/ (nth gcof j) y))))))))

(defn gammp-series [a x gln]
  (loop [ap a del (/ 1.0 a) sm (/ 1.0 a) n 0]
    (let [ap (+ ap 1.0) del (* del (/ x ap)) sm (+ sm del)]
      (if (or (< (absv del) (* (absv sm) geps)) (>= (inc n) 1000))
        (* sm (Math/exp (+ (- x) (* a (Math/log x)) (- gln))))
        (recur ap del sm (inc n))))))

(defn gamma-cf [a x gln upper]
  (loop [i 1 b (+ (+ x 1.0) (- a)) c (/ 1.0 gfpmin) d (/ 1.0 (+ (+ x 1.0) (- a))) h (/ 1.0 (+ (+ x 1.0) (- a)))]
    (let [an (* (- (double i)) (- (double i) a))
          b (+ b 2.0)
          d (+ (* an d) b) d (if (< (absv d) gfpmin) gfpmin d)
          c (+ b (/ an c)) c (if (< (absv c) gfpmin) gfpmin c)
          d (/ 1.0 d) del (* d c) h (* h del)]
      (if (or (<= (absv (- del 1.0)) geps) (>= i 1000))
        (let [q (* (Math/exp (+ (- x) (* a (Math/log x)) (- gln))) h)]
          (if upper q (- 1.0 q)))
        (recur (inc i) b c d h)))))

(defn gammp [a x]
  (if (= x 0.0)
    0.0
    (let [gln (gammln a)]
      (if (< x (+ a 1.0))
        (gammp-series a x gln)
        (gamma-cf a x gln false)))))

(defn gammq [a x]
  (if (= x 0.0)
    1.0
    (let [gln (gammln a)]
      (if (< x (+ a 1.0))
        (- 1.0 (gammp-series a x gln))
        (gamma-cf a x gln true)))))

(defn erfccheb [z]
  (let [t (/ 2.0 (+ 2.0 z)) ty (- (* 4.0 t) 2.0)]
    (loop [j 27 d 0.0 dd 0.0]
      (if (<= j 0)
        (* t (Math/exp (+ (- (* z z)) (* 0.5 (+ (nth erfcof 0) (* ty d))) (- dd))))
        (let [tmp d] (recur (dec j) (+ (- (* ty d) dd) (nth erfcof j)) tmp))))))

(defn erfcx [x]
  (if (>= x 0.0) (erfccheb x) (- 2.0 (erfccheb (- x)))))

(defn inverfc [p]
  (cond
    (>= p 2.0) -100.0
    (<= p 0.0) 100.0
    :else (let [pp (if (< p 1.0) p (- 2.0 p))
                t (Math/sqrt (* -2.0 (Math/log (/ pp 2.0))))
                x0 (* -0.70711 (- (/ (+ 2.30753 (* t 0.27061)) (+ 1.0 (* t (+ 0.99229 (* t 0.04481))))) t))]
            (loop [j 0 x x0]
              (if (>= j 2)
                (if (< p 1.0) x (- x))
                (let [err (- (erfcx x) pp)]
                  (recur (inc j) (+ x (/ err (- (* 1.12837916709551257 (Math/exp (- (* x x)))) (* x err)))))))))))

(defn betacf [a b x]
  (let [qab (+ a b) qap (+ a 1.0) qam (- a 1.0)
        d0 (- 1.0 (/ (* qab x) qap)) d0 (if (< (absv d0) gfpmin) gfpmin d0)]
    (loop [m 1 c 1.0 d (/ 1.0 d0) h (/ 1.0 d0)]
      (let [m2 (* 2 m)
            aa (/ (* (* (double m) (- b (double m))) x) (* (+ qam m2) (+ a m2)))
            d (+ 1.0 (* aa d)) d (if (< (absv d) gfpmin) gfpmin d)
            c (+ 1.0 (/ aa c)) c (if (< (absv c) gfpmin) gfpmin c)
            d (/ 1.0 d) h (* h (* d c))
            aa (/ (* (* (- (+ a (double m))) (+ qab (double m))) x) (* (+ a m2) (+ qap m2)))
            d (+ 1.0 (* aa d)) d (if (< (absv d) gfpmin) gfpmin d)
            c (+ 1.0 (/ aa c)) c (if (< (absv c) gfpmin) gfpmin c)
            d (/ 1.0 d) del (* d c) h (* h del)]
        (if (or (<= (absv (- del 1.0)) geps) (>= m 9999))
          h
          (recur (inc m) c d h))))))

(defn betai [a b x]
  (cond
    (= x 0.0) x
    (= x 1.0) x
    :else (let [bt (Math/exp (+ (- (gammln (+ a b)) (gammln a)) (- (gammln b)) (* a (Math/log x)) (* b (Math/log (- 1.0 x)))))]
            (if (< x (/ (+ a 1.0) (+ (+ a b) 2.0)))
              (/ (* bt (betacf a b x)) a)
              (- 1.0 (/ (* bt (betacf b a (- 1.0 x))) b))))))

(defn invbetai [p a b]
  (let [epsi 1.0e-8]
    (cond
      (<= p 0.0) 0.0
      (>= p 1.0) 1.0
      :else (let [a1 (- a 1.0) b1 (- b 1.0)
                  x0 (if (and (>= a 1.0) (>= b 1.0))
                       (let [pp (if (< p 0.5) p (- 1.0 p))
                             t (Math/sqrt (* -2.0 (Math/log pp)))
                             x (- (/ (+ 2.30753 (* t 0.27061)) (+ 1.0 (* t (+ 0.99229 (* t 0.04481))))) t)
                             x (if (< p 0.5) (- x) x)
                             al (/ (- (* x x) 3.0) 6.0)
                             h (/ 2.0 (+ (/ 1.0 (- (* 2.0 a) 1.0)) (/ 1.0 (- (* 2.0 b) 1.0))))
                             w (- (/ (* x (Math/sqrt (+ al h))) h)
                                    (* (- (/ 1.0 (- (* 2.0 b) 1.0)) (/ 1.0 (- (* 2.0 a) 1.0)))
                                       (+ al (/ 5.0 6.0) (- (/ 2.0 (* 3.0 h))))))]
                         (/ a (+ a (* b (Math/exp (* 2.0 w))))))
                       (let [lna (Math/log (/ a (+ a b)))
                             lnb (Math/log (/ b (+ a b)))
                             t (/ (Math/exp (* a lna)) a)
                             u (/ (Math/exp (* b lnb)) b)
                             w (+ t u)]
                         (if (< p (/ t w))
                           (Math/pow (* (* a w) p) (/ 1.0 a))
                           (- 1.0 (Math/pow (* (* b w) (- 1.0 p)) (/ 1.0 b))))))
                  afac (+ (- (gammln a)) (- (gammln b)) (gammln (+ a b)))]
              (loop [j 0 x x0]
                (if (or (= x 0.0) (= x 1.0))
                  x
                  (let [err (- (betai a b x) p)
                        t (Math/exp (+ (* a1 (Math/log x)) (* b1 (Math/log (- 1.0 x))) afac))
                        u (/ err t)
                        uu (* u (- (/ a1 x) (/ b1 (- 1.0 x))))
                        uu (if (> uu 1.0) 1.0 uu)
                        tt (/ u (- 1.0 (* 0.5 uu)))
                        x (- x tt)
                        x (if (<= x 0.0) (* 0.5 (+ x tt)) x)
                        x (if (>= x 1.0) (* 0.5 (+ (+ x tt) 1.0)) x)]
                    (if (and (< (absv tt) (* epsi x)) (> j 0))
                      x
                      (if (>= (inc j) 10) x (recur (inc j) x))))))))))

(defn invgammp [p a]
  (let [eps 1.0e-8 gln (gammln a) a1 (- a 1.0)]
    (cond
      (>= p 1.0) (Math/max 100.0 (+ a (* 100.0 (Math/sqrt a))))
      (<= p 0.0) 0.0
      :else (let [[x0 lna1 afac] (if (> a 1.0)
                                  (let [lna1 (Math/log a1)
                                        afac (Math/exp (- (* a1 (- lna1 1.0)) gln))
                                        pp (if (< p 0.5) p (- 1.0 p))
                                        t (Math/sqrt (* -2.0 (Math/log pp)))
                                        x (- (/ (+ 2.30753 (* t 0.27061)) (+ 1.0 (* t (+ 0.99229 (* t 0.04481))))) t)
                                        x (if (< p 0.5) (- x) x)]
                                    [(Math/max 1.0e-3 (* a (Math/pow (- (- 1.0 (/ 1.0 (* 9.0 a))) (/ x (* 3.0 (Math/sqrt a)))) 3.0))) lna1 afac])
                                  (let [t (- 1.0 (* a (+ 0.253 (* a 0.12))))]
                                    [(if (< p t)
                                       (Math/pow (/ p t) (/ 1.0 a))
                                       (- 1.0 (Math/log (- 1.0 (/ (- p t) (- 1.0 t)))))) 0.0 0.0]))]
              (loop [j 0 x x0]
                (if (<= x 0.0)
                  0.0
                  (let [err (- (gammp a x) p)
                        t (if (> a 1.0)
                            (* afac (Math/exp (+ (- (- x a1)) (* a1 (- (Math/log x) lna1)))))
                            (Math/exp (+ (- x) (* a1 (Math/log x)) (- gln))))
                        u (/ err t)
                        uu (* u (- (/ (- a 1.0) x) 1.0))
                        uu (if (> uu 1.0) 1.0 uu)
                        tt (/ u (- 1.0 (* 0.5 uu)))
                        x (- x tt)
                        x (if (<= x 0.0) (* 0.5 (+ x tt)) x)]
                    (if (< (absv tt) (* eps x))
                      x
                      (if (>= (inc j) 12) x (recur (inc j) x))))))))))

(defn invxlogx [y]
  (let [ooe 0.367879441171442322
        u0 (if (< y -0.2) (Math/log (- ooe (Math/sqrt (* (* 2.0 ooe) (+ y ooe))))) -10.0)]
    (loop [u u0 to 0.0]
      (let [t (* (- (Math/log (/ y u)) u) (/ u (+ 1.0 u)))
            nu (+ u t)]
        (if (and (< t 1.0e-8) (< (absv (+ t to)) (* 0.01 (absv t))))
          (Math/exp nu)
          (if (> (absv (/ t nu)) 1.0e-15) (recur nu t) (Math/exp nu)))))))

(defn pks [z]
  (cond
    (= z 0.0) 0.0
    (< z 1.18) (let [y (Math/exp (/ -1.23370055013616983 (* z z)))]
                 (* (* 2.25675833419102515 (Math/sqrt (- (Math/log y))))
                    (+ (+ (+ y (Math/pow y 9.0)) (Math/pow y 25.0)) (Math/pow y 49.0))))
    :else (let [x (Math/exp (* -2.0 (* z z)))]
            (- 1.0 (* 2.0 (+ (- x (Math/pow x 4.0)) (Math/pow x 9.0)))))))

(defn qks [z]
  (cond
    (= z 0.0) 1.0
    (< z 1.18) (- 1.0 (pks z))
    :else (let [x (Math/exp (* -2.0 (* z z)))]
            (* 2.0 (+ (- x (Math/pow x 4.0)) (Math/pow x 9.0))))))

(defn invqks [q]
  (cond
    (= q 1.0) 0.0
    (> q 0.3) (let [t1 (- 1.0 q)
                    f (* (* -0.392699081698724155 t1) t1)
                    y0 (invxlogx f)]
                (loop [y y0]
                  (let [logy (Math/log y)
                        s (+ (+ 1.0 (Math/pow y 4.0)) (Math/pow y 12.0))
                        ff (/ f (* s s))
                        u (/ (- (* y logy) ff) (+ 1.0 logy))
                        t (/ u (Math/max 0.5 (- 1.0 (/ (* 0.5 u) (* y (+ 1.0 logy))))))
                        ny (- y t)]
                    (if (> (absv (/ t ny)) 1.0e-15)
                      (recur ny)
                      (/ 1.57079632679489662 (Math/sqrt (- (Math/log ny))))))))
    :else (loop [x 0.03]
            (let [xp x
                  x (- (+ (* 0.5 q) (Math/pow x 4.0)) (Math/pow x 9.0))
                  x (if (> x 0.06) (+ x (- (Math/pow x 16.0) (Math/pow x 25.0))) x)]
              (if (> (absv (/ (- xp x) x)) 1.0e-15)
                (recur x)
                (Math/sqrt (* -0.5 (Math/log x))))))))

(defn poisson-cdf [lam n]
  (if (= n 0) 0.0 (gammq (double n) lam)))

(defn poisson-inv [lam p]
  (if (< p (Math/exp (- lam)))
    0
    (let [n0 (long (Math/max (Math/sqrt lam) 5.0))]
      (loop [n n0 inc 1 up (not (< p (poisson-cdf lam n0)))]
        (if (not up)
          (let [n (if (> (- n inc) 0) (- n inc) 0) inc (* inc 2)]
            (if (< p (poisson-cdf lam n))
              (recur n inc false)
              (loop [lo n hi (+ n (quot inc 2))]
                (if (<= (- hi lo) 1)
                  lo
                  (let [m (quot (+ lo hi) 2)]
                    (if (< p (poisson-cdf lam m)) (recur lo m) (recur m hi)))))))
          (let [n (+ n inc) inc (* inc 2)]
            (if (> p (poisson-cdf lam n))
              (recur n inc true)
              (loop [lo (- n (quot inc 2)) hi n]
                (if (<= (- hi lo) 1)
                  lo
                  (let [m (quot (+ lo hi) 2)]
                    (if (< p (poisson-cdf lam m)) (recur lo m) (recur m hi))))))))))))

(defn binomial-cdf [n pe k]
  (cond
    (= k 0) 0.0
    (> k n) 1.0
    :else (- 1.0 (betai (double k) (double (+ (- n k) 1)) pe))))

(defn binomial-inv [n pe p]
  (let [k0 (long (* (double n) pe))
        k0 (max 0 (min n k0))]
    (loop [k k0 inc 1 up (not (< p (binomial-cdf n pe k0)))]
      (if (not up)
        (let [k (if (> (- k inc) 0) (- k inc) 0) inc (* inc 2)]
          (if (< p (binomial-cdf n pe k))
            (recur k inc false)
            (loop [lo k hi (+ k (quot inc 2))]
              (if (<= (- hi lo) 1)
                lo
                (let [m (quot (+ lo hi) 2)]
                  (if (< p (binomial-cdf n pe m)) (recur lo m) (recur m hi)))))))
        (let [k (if (< (+ k inc) (+ n 1)) (+ k inc) (+ n 1)) inc (* inc 2)]
          (if (> p (binomial-cdf n pe k))
            (recur k inc true)
            (loop [lo (- k (quot inc 2)) hi k]
              (if (<= (- hi lo) 1)
                lo
                (let [m (quot (+ lo hi) 2)]
                  (if (< p (binomial-cdf n pe k)) (recur lo m) (recur m hi)))))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          f (fn [i] (Double/parseDouble (nth toks i)))
          li (fn [i] (Long/parseLong (nth toks i)))]
      (cond
        (= op "NORMAL") (let [mu (f 1) sig (f 2) x (f 3)]
                          (println (fmt6 (canon (* 0.5 (erfcx (/ (* -0.707106781186547524 (- x mu)) sig)))))))
        (= op "NORMAL_INV") (let [mu (f 1) sig (f 2) p (f 3)]
                              (println (fmt6 (canon (+ (* (* -1.41421356237309505 sig) (inverfc (* 2.0 p))) mu)))))
        (= op "CAUCHY") (let [mu (f 1) sig (f 2) x (f 3)]
                          (println (fmt6 (canon (+ 0.5 (* 0.318309886183790671 (Math/atan2 (- x mu) sig)))))))
        (= op "CAUCHY_INV") (let [mu (f 1) sig (f 2) p (f 3)]
                              (println (fmt6 (canon (+ mu (* sig (Math/tan (* 3.14159265358979324 (- p 0.5)))))))))
        (= op "STUDENTT") (let [nu (f 1) mu (f 2) sig (f 3) t (f 4)
                                z (/ (- t mu) sig)
                                p (* 0.5 (betai (* 0.5 nu) 0.5 (/ nu (+ nu (* z z)))))]
                            (println (fmt6 (canon (if (>= t mu) (- 1.0 p) p)))))
        (= op "STUDENTT_INV") (let [nu (f 1) mu (f 2) sig (f 3) p (f 4)
                                    x (invbetai (* 2.0 (Math/min p (- 1.0 p))) (* 0.5 nu) 0.5)
                                    x (* sig (Math/sqrt (/ (* nu (- 1.0 x)) x)))]
                                (println (fmt6 (canon (if (>= p 0.5) (+ mu x) (- mu x))))))
        (= op "STUDENTT_AA") (let [nu (f 1) t (f 2)]
                               (println (fmt6 (canon (- 1.0 (betai (* 0.5 nu) 0.5 (/ nu (+ nu (* t t)))))))))
        (= op "STUDENTT_INVAA") (let [nu (f 1) p (f 2)
                                      x (invbetai (- 1.0 p) (* 0.5 nu) 0.5)]
                                  (println (fmt6 (canon (Math/sqrt (/ (* nu (- 1.0 x)) x))))))
        (= op "LOGISTIC") (let [mu (f 1) sig (f 2) x (f 3)
                                e (Math/exp (- (absv (/ (* 1.81379936423421785 (- x mu)) sig))))]
                            (println (fmt6 (canon (if (>= x mu) (/ 1.0 (+ 1.0 e)) (/ e (+ 1.0 e)))))))
        (= op "LOGISTIC_INV") (let [mu (f 1) sig (f 2) p (f 3)]
                                (println (fmt6 (canon (+ mu (* (* 0.551328895421792049 sig) (Math/log (/ p (- 1.0 p)))))))))
        (= op "EXPON") (let [bet (f 1) x (f 2)]
                         (println (fmt6 (canon (- 1.0 (Math/exp (* (- bet) x)))))))
        (= op "EXPON_INV") (let [bet (f 1) p (f 2)]
                             (println (fmt6 (canon (/ (- (Math/log (- 1.0 p))) bet)))))
        (= op "WEIBULL") (let [alph (f 1) bet (f 2) x (f 3)]
                           (println (fmt6 (canon (- 1.0 (Math/exp (- (Math/pow (/ x bet) alph))))))))
        (= op "WEIBULL_INV") (let [alph (f 1) bet (f 2) p (f 3)]
                               (println (fmt6 (canon (* bet (Math/pow (- (Math/log (- 1.0 p))) (/ 1.0 alph)))))))
        (= op "LOGNORMAL") (let [mu (f 1) sig (f 2) x (f 3)]
                             (println (fmt6 (canon (if (= x 0.0) 0.0 (* 0.5 (erfcx (/ (* -0.707106781186547524 (- (Math/log x) mu)) sig))))))))
        (= op "LOGNORMAL_INV") (let [mu (f 1) sig (f 2) p (f 3)]
                                 (println (fmt6 (canon (Math/exp (+ (* (* -1.41421356237309505 sig) (inverfc (* 2.0 p))) mu))))))
        (= op "CHISQ") (let [nu (f 1) x (f 2)]
                         (println (fmt6 (canon (gammp (* 0.5 nu) (* 0.5 x))))))
        (= op "CHISQ_INV") (let [nu (f 1) p (f 2)]
                             (println (fmt6 (canon (* 2.0 (invgammp p (* 0.5 nu)))))))
        (= op "GAMMA") (let [alph (f 1) bet (f 2) x (f 3)]
                         (println (fmt6 (canon (gammp alph (* bet x))))))
        (= op "GAMMA_INV") (let [alph (f 1) bet (f 2) p (f 3)]
                             (println (fmt6 (canon (/ (invgammp p alph) bet)))))
        (= op "FDIST") (let [nu1 (f 1) nu2 (f 2) ff (f 3)]
                         (println (fmt6 (canon (betai (* 0.5 nu1) (* 0.5 nu2) (/ (* nu1 ff) (+ nu2 (* nu1 ff))))))))
        (= op "FDIST_INV") (let [nu1 (f 1) nu2 (f 2) p (f 3)
                                 x (invbetai p (* 0.5 nu1) (* 0.5 nu2))]
                             (println (fmt6 (canon (/ (* nu2 x) (* nu1 (- 1.0 x)))))))
        (= op "BETA") (let [alph (f 1) bet (f 2) x (f 3)]
                        (println (fmt6 (canon (betai alph bet x)))))
        (= op "BETA_INV") (let [alph (f 1) bet (f 2) p (f 3)]
                            (println (fmt6 (canon (invbetai p alph bet)))))
        (= op "KS_P") (println (fmt6 (canon (pks (f 1)))))
        (= op "KS_Q") (println (fmt6 (canon (qks (f 1)))))
        (= op "KS_INVQ") (println (fmt6 (canon (invqks (f 1)))))
        (= op "KS_INVP") (println (fmt6 (canon (invqks (- 1.0 (f 1))))))
        (= op "POISSON") (let [lam (f 1) n (li 2)]
                           (println (fmt6 (canon (poisson-cdf lam n)))))
        (= op "POISSON_INV") (println (poisson-inv (f 1) (f 2)))
        (= op "BINOMIAL") (let [n (li 1) pe (f 2) k (li 3)]
                            (println (fmt6 (canon (binomial-cdf n pe k)))))
        (= op "BINOMIAL_INV") (println (binomial-inv (li 1) (f 2) (f 3)))))))
