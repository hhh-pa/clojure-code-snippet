;; Maximum-entropy (all-poles) power spectrum from Burg LP
;; coefficients by evaluating the prediction-error filter on the
;; unit circle.
;; Reference: Press et al., Numerical Recipes 3e, Section 13.8
;; (evlmem; takes memcof-style (d, xms) as explicit input).
;; Documented decisions: the book's literal 6.28318530717959 is
;; used verbatim for theta; a zero denominator is unguarded in
;; the book (fixtures keep it positive).
;; No online-judge problem found -- fixtures are self-generated
;; (numpy oracle), no header link.
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

(defn evlmem [fdt ^doubles d xms]
  (let [m (alength ^doubles d)
        theta (* 6.28318530717959 fdt)
        wtemp (Math/sin (* 0.5 theta))
        wpr (* (* -2.0 wtemp) wtemp)
        wpi (Math/sin theta)]
    (loop [i 0 wr 1.0 wi 0.0 sumr 1.0 sumi 0.0]
      (if (>= i m)
        (/ xms (+ (* sumr sumr) (* sumi sumi)))
        (let [wtemp wr
              wr (+ (- (* wtemp wpr) (* wi wpi)) wr)
              wi (+ (+ (* wi wpr) (* wtemp wpi)) wi)]
          (recur (inc i) wr wi
                 (- sumr (* (aget ^doubles d i) wr))
                 (- sumi (* (aget ^doubles d i) wi))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (when (= op "MEMSPEC")
        (let [m (Long/parseLong (nth toks 1))
              k (Long/parseLong (nth toks 2))
              mm (max 0 m)
              kk (max 0 k)
              d (double-array (map #(Double/parseDouble %) (subvec toks 3 (+ 3 mm))))
              xms (Double/parseDouble (nth toks (+ 3 mm)))
              fs (map #(Double/parseDouble %) (subvec toks (+ 4 mm) (+ 4 mm kk)))]
          (println (str (str/join " " (concat (map #(fmt6 (canon (evlmem % d xms))) fs)
                                              ["0"])))))))))
