;; Incomplete gamma and error functions (NR 6.2).
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

(def erfcof [-1.3026537197817094 6.4196979235649026e-1 1.9476473204185836e-2
             -9.561514786808631e-3 -9.46595344482036e-4 3.66839497852761e-4
             4.2523324806907e-5 -2.0278578112534e-5 -1.624290004647e-6
             1.303655835580e-6 1.5626441722e-8 -8.5238095915e-8
             6.529054439e-9 5.059343495e-9 -9.91364156e-10
             -2.27365122e-10 9.6467911e-11 2.394038e-12
             -6.886027e-12 8.94487e-13 3.13092e-13
             -1.12708e-13 3.81e-16 7.106e-15
             -1.523e-15 -9.4e-17 1.21e-16 -2.8e-17])

(defn erfccheb [z]
  (let [t (/ 2.0 (+ 2.0 z))
        ty (- (* 4.0 t) 2.0)
        [d dd] (loop [j 27 d 0.0 dd 0.0]
                 (if (<= j 0) [d dd]
                     (recur (dec j) (+ (- (* ty d) dd) (nth erfcof j)) d)))]
    (* t (Math/exp (+ (* (- z) z) (* 0.5 (+ (nth erfcof 0) (* ty d))) (- dd))))))

(defn erfcx [x]
  (if (>= x 0.0) (erfccheb x) (- 2.0 (erfccheb (- x)))))

(defn inverfc [p]
  (cond (>= p 2.0) -100.0
        (<= p 0.0) 100.0
        :else (let [pp (if (< p 1.0) p (- 2.0 p))
                    t (Math/sqrt (* -2.0 (Math/log (/ pp 2.0))))
                    x (* -0.70711 (- (/ (+ 2.30753 (* t 0.27061)) (+ 1.0 (* t (+ 0.99229 (* t 0.04481))))) t))]
                (loop [j 0 x x]
                  (if (>= j 2) (if (< p 1.0) x (- x))
                      (let [err (- (erfcx x) pp)]
                        (recur (inc j) (+ x (/ err (- (* 1.12837916709551257 (Math/exp (- (* x x)))) (* x err)))))))))))

(def eps 2.220446049250313e-16)
(def fpmin 1.0020841800044864e-292)

(defn gser [a x gln]
  (let [fin (fn [s] (* s (Math/exp (+ (- x) (* a (Math/log x)) (- gln)))))]
    (loop [n 0 ap a del (/ 1.0 a) sum (/ 1.0 a)]
      (if (>= n 1000) (fin sum)
          (let [ap (inc ap)
                del (* del (/ x ap))
                sum (+ sum del)]
            (if (< (absv del) (* (absv sum) eps))
              (fin sum)
              (recur (inc n) ap del sum)))))))

(defn gcf [a x gln]
  (let [fin (fn [h] (* (Math/exp (+ (- x) (* a (Math/log x)) (- gln))) h))]
    (loop [i 1 b (+ x 1.0 (- a)) c (/ 1.0 fpmin) d (/ 1.0 (+ x 1.0 (- a))) h (/ 1.0 (+ x 1.0 (- a)))]
      (if (> i 1000) (fin h)
          (let [an (* (- i) (- i a))
                b (+ b 2.0)
                d (+ (* an d) b)
                d (if (< (absv d) fpmin) fpmin d)
                c (+ b (/ an c))
                c (if (< (absv c) fpmin) fpmin c)
                d (/ 1.0 d)
                del (* d c)
                h (* h del)]
            (if (<= (absv (- del 1.0)) eps)
              (fin h)
              (recur (inc i) b c d h)))))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          ans (cond (or (= op "GAMMP") (= op "GAMMQ"))
                    (let [a (Double/parseDouble (nth toks 1))
                          x (Double/parseDouble (nth toks 2))
                          gln (gammln a)]
                      (cond (zero? x) (if (= op "GAMMP") 0.0 1.0)
                            (< x (+ a 1.0)) (let [p (gser a x gln)]
                                              (if (= op "GAMMP") p (- 1.0 p)))
                            :else (let [q (gcf a x gln)]
                                    (if (= op "GAMMP") (- 1.0 q) q))))
                    (= op "ERF") (let [x (Double/parseDouble (nth toks 1))]
                                   (if (>= x 0.0) (- 1.0 (erfccheb x)) (- (erfccheb (- x)) 1.0)))
                    (= op "ERFC") (erfcx (Double/parseDouble (nth toks 1)))
                    (= op "INVERFC") (inverfc (Double/parseDouble (nth toks 1)))
                    :else (inverfc (- 1.0 (Double/parseDouble (nth toks 1)))))]
      (println (fmt6 (canon ans))))))
