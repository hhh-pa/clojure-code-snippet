;; Gamma/beta/factorial/binomial (NR 6.1).
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

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          fact (loop [i 1 v (assoc (vec (repeat 171 0.0)) 0 1.0)]
                 (if (>= i 171) v (recur (inc i) (assoc v i (* i (nth v (dec i)))))))
          factl (vec (for [i (range 2000)] (gammln (+ i 1.0))))
          ans (cond (= op "GAMMALN") (gammln (Double/parseDouble (nth toks 1)))
                    (= op "GAMMA") (Math/exp (gammln (Double/parseDouble (nth toks 1))))
                    (= op "BETA") (Math/exp (+ (- (+ (gammln (Double/parseDouble (nth toks 1)))
                                                     (gammln (Double/parseDouble (nth toks 2))))
                                                  (gammln (+ (Double/parseDouble (nth toks 1))
                                                             (Double/parseDouble (nth toks 2)))))))
                    (= op "FACT") (nth fact (Long/parseLong (nth toks 1)))
                    (= op "FACTLN") (let [n (Long/parseLong (nth toks 1))]
                                      (if (< n 2000) (nth factl n) (gammln (+ n 1.0))))
                    :else (let [n (Long/parseLong (nth toks 1)) k (Long/parseLong (nth toks 2))]
                            (if (< n 171)
                              (Math/floor (+ 0.5 (/ (nth fact n) (* (nth fact k) (nth fact (- n k))))))
                              (Math/floor (+ 0.5 (Math/exp (- (- (nth factl n) (nth factl k)) (nth factl (- n k)))))))))]
      (println (fmt6 (canon ans))))))
