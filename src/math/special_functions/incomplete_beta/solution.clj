;; Incomplete beta function (NR 6.4).
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

(def gcof [57.1562356658629235 -59.5979603554754912 14.1360979747417471
           -0.491913816097620199 0.339946499848118887e-4 0.465236289270485756e-4
           -0.983744753048795646e-4 0.158088703224912494e-3 -0.210264441724104883e-3
           0.217439618115212643e-3 -0.164318106536763890e-3 0.844182239838527433e-4
           -0.261908384015814087e-4 0.368991826595316234e-5])

(defn gammln [xx]
  (let [tmp (- (* (+ xx 0.5) (Math/log (+ xx 5.2421875000000000))) (+ xx 5.2421875000000000))
        ser (loop [j 0 y xx ser 0.999999999999997092]
              (if (>= j 14) ser
                  (let [y (inc y)] (recur (inc j) y (+ ser (/ (nth gcof j) y))))))]
    (+ tmp (Math/log (/ (* 2.5066282746310005 ser) xx)))))

(def eps 2.220446049250313e-16)
(def fpmin 1.0020841800044864e-292)

(defn betacf [a b x]
  (let [qab (+ a b)
        qap (+ a 1.0)
        qam (- a 1.0)
        d0 (- 1.0 (/ (* qab x) qap))
        d0 (if (< (absv d0) fpmin) fpmin d0)
        d (/ 1.0 d0)]
    (loop [m 1 c 1.0 d d h d]
      (if (>= m 10000) h
          (let [m2 (* 2 m)
                aa (/ (* m (- b m) x) (* (+ qam m2) (+ a m2)))
                d (+ 1.0 (* aa d))
                d (if (< (absv d) fpmin) fpmin d)
                c (+ 1.0 (/ aa c))
                c (if (< (absv c) fpmin) fpmin c)
                d (/ 1.0 d)
                h (* h d c)
                aa (/ (* (- (+ a m)) (+ qab m) x) (* (+ a m2) (+ qap m2)))
                d (+ 1.0 (* aa d))
                d (if (< (absv d) fpmin) fpmin d)
                c (+ 1.0 (/ aa c))
                c (if (< (absv c) fpmin) fpmin c)
                d (/ 1.0 d)
                del (* d c)
                h (* h del)]
            (if (<= (absv (- del 1.0)) eps) h (recur (inc m) c d h)))))))

(defn betai [a b x]
  (cond (or (zero? x) (= x 1.0)) x
        :else (let [t1 (- (gammln (+ a b)) (gammln a))
                    t2 (- t1 (gammln b))
                    t3 (+ t2 (* a (Math/log x)))
                    t4 (+ t3 (* b (Math/log (- 1.0 x))))
                    bt (Math/exp t4)]
                (if (< x (/ (+ a 1.0) (+ a b 2.0)))
                  (/ (* bt (betacf a b x)) a)
                  (- 1.0 (/ (* bt (betacf b a (- 1.0 x))) b))))))

(defn invbetai [p a b]
  (let [epsi 1.0e-8]
    (cond (<= p 0.0) 0.0
          (>= p 1.0) 1.0
          :else (let [a1 (- a 1.0)
                      b1 (- b 1.0)
                      x (if (and (>= a 1.0) (>= b 1.0))
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
                              (Math/pow (* a w p) (/ 1.0 a))
                              (- 1.0 (Math/pow (* b w (- 1.0 p)) (/ 1.0 b))))))
                      afac (+ (- (- (gammln a)) (gammln b)) (gammln (+ a b)))]
                  (loop [j 0 x x]
                    (if (or (zero? x) (= x 1.0)) x
                        (if (>= j 10) x
                            (let [err (- (betai a b x) p)
                                  t (Math/exp (+ (* a1 (Math/log x)) (* b1 (Math/log (- 1.0 x))) afac))
                                  u (/ err t)
                                  uu0 (* u (- (/ a1 x) (/ b1 (- 1.0 x))))
                                  uu (if (> uu0 1.0) 1.0 uu0)
                                  tt (/ u (- 1.0 (* 0.5 uu)))
                                  x (- x tt)
                                  x (if (<= x 0.0) (* 0.5 (+ x tt)) x)
                                  x (if (>= x 1.0) (* 0.5 (+ x tt 1.0)) x)]
                              (if (and (< (absv tt) (* epsi x)) (> j 0)) x (recur (inc j) x))))))))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          ans (if (= op "BETAI")
                (betai (Double/parseDouble (nth toks 1)) (Double/parseDouble (nth toks 2)) (Double/parseDouble (nth toks 3)))
                (invbetai (Double/parseDouble (nth toks 1)) (Double/parseDouble (nth toks 2)) (Double/parseDouble (nth toks 3))))]
      (println (fmt6 (canon ans))))))
