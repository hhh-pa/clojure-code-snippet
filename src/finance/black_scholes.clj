(require '[clojure.string :as str])
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn erf [x]
  (let [a1 0.254829592 a2 -0.284496736 a3 1.421413741 a4 -1.453152027 a5 1.061405429 p 0.3275911
        sign (if (< x 0) -1.0 1.0) ax (Math/abs (double x)) t (/ 1.0 (+ 1.0 (* p ax)))
        poly (+ (* (+ (* (+ (* (+ (* a5 t) a4) t) a3) t) a2) t) a1)
        y (- 1.0 (* poly t (Math/exp (- (* ax ax)))))]
    (* sign y)))
(defn ncdf [x] (* 0.5 (+ 1.0 (erf (/ x (Math/sqrt 2))))))
(defn npdf [x] (/ (Math/exp (* -0.5 x x)) (Math/sqrt (* 2 Math/PI))))
(defn -main [input]
  (when-not (str/blank? input)
    (let [toks (str/split (str/trim input) #"\s+")
          S (Double/parseDouble (nth toks 0)) K (Double/parseDouble (nth toks 1)) T (Double/parseDouble (nth toks 2))
          r (Double/parseDouble (nth toks 3)) sig (Double/parseDouble (nth toks 4)) tp (nth toks 5)
          d1 (/ (+ (Math/log (/ S K)) (* (+ r (* 0.5 sig sig)) T)) (* sig (Math/sqrt T)))
          d2 (- d1 (* sig (Math/sqrt T)))
          price (if (= tp "call") (- (* S (ncdf d1)) (* K (Math/exp (* (- r) T)) (ncdf d2))) (- (* K (Math/exp (* (- r) T)) (ncdf (- d2))) (* S (ncdf (- d1)))))
          delta (if (= tp "call") (ncdf d1) (- (ncdf d1) 1.0))
          gamma (/ (npdf d1) (* S sig (Math/sqrt T)))
          vega (* S (npdf d1) (Math/sqrt T))
          theta (+ (/ (- (* S (npdf d1) sig)) (* 2 (Math/sqrt T))) (if (= tp "call") (- (* r K (Math/exp (* (- r) T)) (ncdf d2))) (* r K (Math/exp (* (- r) T)) (ncdf (- d2)))))
          rho (if (= tp "call") (* K T (Math/exp (* (- r) T)) (ncdf d2)) (- (* K T (Math/exp (* (- r) T)) (ncdf (- d2)))))]
      (println (fmt "%.4f" price)) (println (fmt "%.4f" delta)) (println (fmt "%.4f" gamma))
      (println (fmt "%.4f" vega)) (println (fmt "%.4f" theta)) (println (fmt "%.4f" rho)))))
(-main (slurp *in*))
