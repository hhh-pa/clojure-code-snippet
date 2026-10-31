;; merton_vasicek: Merton structural credit model (equity-implied assets,
;; distance to default, PD via Newton/fixed-point) & Vasicek one-factor
;; Gaussian copula CDO tranche expected loss via 10-point Gauss-Hermite
;; quadrature. Self-contained erf (AS 7.1.26) & Acklam inverse-normal.
;; Batch 6 (T384). Input: E sE D r T, then p rho att det.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn erf7 [x]
  (let [a1 0.254829592 a2 -0.284496736 a3 1.421413741 a4 -1.453152027 a5 1.061405429 p 0.3275911
        s (if (< x 0) -1.0 1.0) ax (Math/abs x) t (/ 1.0 (+ 1.0 (* p ax)))
        poly (+ (* (+ (* (+ (* (+ (* (+ (* a5 t) a4) t) a3) t) a2) t) a1)))]
    (* s (- 1.0 (* poly t (Math/exp (- (* ax ax))))))))
(defn ncdf [x] (* 0.5 (+ 1.0 (erf7 (/ x (Math/sqrt 2.0))))))
(defn phiinv [p]
  (let [a1 -3.969683028665376e+01 a2 2.209460984245205e+02 a3 -2.759285104469687e+02
        a4 1.383577518672690e+02 a5 -3.066479806614716e+01 a6 2.506628277459239e+00
        b1 -5.447609879822406e+01 b2 1.615858368580409e+02 b3 -1.556989798598866e+02
        b4 6.680131188771972e+01 b5 -1.328068155288572e+01
        c1 -7.784894002430293e-03 c2 -3.223964580411365e-01 c3 -2.400758277161838e+00
        c4 -2.549732539343734e+00 c5 4.374664141464968e+00 c6 2.938163982698783e+00
        d1 7.784695709041462e-03 d2 3.224671290700398e-01 d3 2.445134137142996e+00 d4 3.754408661907416e+00
        horn5 (fn [k1 k2 k3 k4 k5 k6 q] (+ (* (+ (* (+ (* (+ (* (+ (* k1 q) k2) q) k3) q) k4) q) k5) q) k6))
        horn4 (fn [k1 k2 k3 k4 q] (+ (* (+ (* (+ (* (+ (* k1 q) k2) q) k3) q) k4) q) 1.0))]
    (cond (< p 0.02425)
      (let [q (Math/sqrt (* -2.0 (Math/log p)))]
        (/ (horn5 c1 c2 c3 c4 c5 c6 q) (horn4 d1 d2 d3 d4 q)))
      (<= p 0.97575)
      (let [q (- p 0.5) r (* q q)]
        (/ (* (horn5 a1 a2 a3 a4 a5 a6 r) q) (horn5 b1 b2 b3 b4 b5 1.0 r)))
      :else
      (let [q (Math/sqrt (* -2.0 (Math/log (- 1.0 p))))]
        (- (/ (horn5 c1 c2 c3 c4 c5 c6 q) (horn4 d1 d2 d3 d4 q)))))))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [E (Double/parseDouble (nth toks 0)) sE (Double/parseDouble (nth toks 1))
            D (Double/parseDouble (nth toks 2)) rr (Double/parseDouble (nth toks 3))
            T (Double/parseDouble (nth toks 4))
            res (loop [it 0 sA (/ (* sE E) (+ E D)) A (+ E (* D (Math/exp (* (- rr) T))))]
                  (if (>= it 100) [A sA]
                    (let [A2 (loop [nt 0 a A]
                               (if (>= nt 50) a
                                 (let [d1 (/ (+ (Math/log (/ a D)) (* (+ rr (* 0.5 sA sA)) T)) (* sA (Math/sqrt T)))
                                       d2 (- d1 (* sA (Math/sqrt T)))
                                       C (- (* a (ncdf d1)) (* D (Math/exp (* (- rr) T)) (ncdf d2)))
                                       delta (ncdf d1)]
                                   (recur (inc nt) (- a (/ (- C E) delta))))))
                          d1 (/ (+ (Math/log (/ A2 D)) (* (+ rr (* 0.5 sA sA)) T)) (* sA (Math/sqrt T)))]
                      (recur (inc it) (/ (* sE E) (* (ncdf d1) A2)) A2))))
            [A sA] res
            DD (/ (+ (Math/log (/ A D)) (* (- rr (* 0.5 sA sA)) T)) (* sA (Math/sqrt T)))
            PD (ncdf (- DD))
            p (Double/parseDouble (nth toks 5)) rho (Double/parseDouble (nth toks 6))
            att (Double/parseDouble (nth toks 7)) det (Double/parseDouble (nth toks 8))
            gx [0.3429013272237046 1.036610829789514 1.756683649299882 2.53273167423279 3.436159118837738]
            gw [0.6108626337353258 0.2401386110823147 0.03387439445548106 0.001343645746781232 7.64043285523262e-06]
            pinv (phiinv p) sqr (Math/sqrt rho) sq1 (Math/sqrt (- 1.0 rho))
            acc (loop [i 0 a 0.0]
                  (if (>= i 5) a
                    (recur (inc i) (loop [s -1 b a]
                                     (if (> s 1) b
                                       (let [z (* s (Math/sqrt 2.0) (nth gx i))
                                             P (ncdf (/ (- pinv (* sqr z)) sq1))
                                             L (max 0.0 (min 1.0 (/ (- P att) (- det att))))]
                                         (recur (+ s 2) (+ b (* (nth gw i) L)))))))))
            el (/ acc (Math/sqrt Math/PI))]
        (println (str "MERTON: " (fmt "%.6f" A) " " (fmt "%.6f" sA) " " (fmt "%.6f" DD) " " (fmt "%.6f" PD)))
        (println (str "VASICEK: " (fmt "%.6f" el)))))))
(-main (slurp *in*))
