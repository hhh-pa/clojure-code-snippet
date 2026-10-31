;; deflated_sharpe: Bailey-Lopez de Prado PSR(SR*) = Phi((SR-SR*)sqrt(T-1)/
;; sqrt(1-g3*SR+(g4-1)/4*SR^2)) and DSR = PSR(SR0) with SR0 from the K
;; trial Sharpes. Phi via Abramowitz-Stegun erf, PhiInv via Acklam.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn aerf [x]
  (let [s (if (< x 0) -1.0 1.0) x (Math/abs (double x))
        t (/ 1.0 (+ 1.0 (* 0.3275911 x)))
        y (- 1.0 (* (+ (* (+ (* (+ (* (+ (* 1.061405429 t) -1.453152027) t) 1.421413741) t) -0.284496736) t) 0.254829592) t (Math/exp (* (- x) x))))]
    (* s y)))
(defn Phi [x] (* 0.5 (+ 1.0 (aerf (/ x (Math/sqrt 2.0))))))
(defn PhiInv [p]
  (let [a1 -3.969683028665376e+01 a2 2.209460984245205e+02 a3 -2.759285104469687e+02
        a4 1.383577518672690e+02 a5 -3.066479806614716e+01 a6 2.506628277459239e+00
        b1 -5.447609879822406e+01 b2 1.615858368580409e+02 b3 -1.556989798598866e+02
        b4 6.680131188771972e+01 b5 -1.328068155288572e+01
        c1 -7.784894002430293e-03 c2 -3.223964580411365e-01 c3 -2.400758277161838e+00
        c4 -2.549732539343734e+00 c5 4.374664141464968e+00 c6 2.938163982698783e+00
        d1 7.784695709041462e-03 d2 3.224671290700398e-01 d3 2.445134137142996e+00 d4 3.754408661907416e+00]
    (cond (< p 0.02425)
          (let [q (Math/sqrt (* -2.0 (Math/log p)))]
            (/ (+ (* (+ (* (+ (* (+ (* (+ (* c1 q) c2) q) c3) q) c4) q) c5) q) c6)
               (+ (* (+ (* (+ (* (+ (* d1 q) d2) q) d3) q) d4) q) 1.0)))
          (> p 0.97575)
          (let [q (Math/sqrt (* -2.0 (Math/log (- 1.0 p))))]
            (- (/ (+ (* (+ (* (+ (* (+ (* (+ (* c1 q) c2) q) c3) q) c4) q) c5) q) c6)
                  (+ (* (+ (* (+ (* (+ (* d1 q) d2) q) d3) q) d4) q) 1.0))))
          :else (let [q (- p 0.5) r (* q q)]
                  (/ (* (+ (* (+ (* (+ (* (+ (* (+ (* a1 r) a2) r) a3) r) a4) r) a5) r) a6) q)
                     (+ (* (+ (* (+ (* (+ (* (+ (* b1 r) b2) r) b3) r) b4) r) b5) r) 1.0))))))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [N (Long/parseLong (nth toks 0)) K (Long/parseLong (nth toks 1))
            bench (Double/parseDouble (nth toks 2))
            r (mapv (fn [k] (Double/parseDouble (nth toks (+ 3 k)))) (range N))
            tr (mapv (fn [k] (Double/parseDouble (nth toks (+ 3 N k)))) (range K))
            m (/ (reduce + r) N)
            acc (loop [i 0 v 0.0 m2 0.0 m3 0.0 m4 0.0]
                  (if (>= i N) [v m2 m3 m4]
                    (let [d (- (nth r i) m)]
                      (recur (inc i) (+ v (* d d)) (+ m2 (* d d)) (+ m3 (* d d d)) (+ m4 (* d d d d))))))
            v (/ (nth acc 0) (- N 1)) m2 (/ (nth acc 1) N) m3 (/ (nth acc 2) N) m4 (/ (nth acc 3) N)
            SR (* (/ m (Math/sqrt v)) (Math/sqrt 252.0))
            g3 (/ m3 (Math/pow m2 1.5)) g4 (/ m4 (* m2 m2))
            den0 (+ (- 1.0 (* g3 SR)) (* (/ (- g4 1.0) 4.0) SR SR))
            den (if (<= den0 0) 1e-12 den0)
            sc (/ (Math/sqrt (- N 1)) (Math/sqrt den))
            PSR (Phi (* (- SR bench) sc))
            tm (/ (reduce + tr) K)
            V (if (> K 1) (/ (reduce + (map (fn [x] (* (- x tm) (- x tm))) tr)) (- K 1)) 0.0)
            SR0 (if (>= K 2)
                  (let [g 0.5772156649015329 e 2.718281828459045
                        a1 (PhiInv (- 1.0 (/ 1.0 K))) a2 (PhiInv (- 1.0 (/ 1.0 (* K e))))]
                    (* (Math/sqrt V) (+ (* (- 1.0 g) a1) (* g a2))))
                  0.0)
            DSR (Phi (* (- SR SR0) sc))]
        (println (fmt "SR: %.6f" (+ SR 0.0)))
        (println (fmt "SKEW: %.6f KURT: %.6f" (+ g3 0.0) (+ g4 0.0)))
        (println (fmt "PSR: %.6f" (+ PSR 0.0)))
        (println (fmt "DSR: %.6f SR0: %.6f" (+ DSR 0.0) (+ SR0 0.0)))))))
(-main (slurp *in*))
