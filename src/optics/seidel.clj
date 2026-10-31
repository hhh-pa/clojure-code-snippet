;; seidel: Seidel third-order aberration sums — paraxial marginal + chief ray
;; trace, then S1..S5 (spherical, coma, astigmatism, Petzval, distortion)
;; and chromatic C1 (axial) C2 (lateral). Batch 6 (T388).
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn sn [x] (if (< (Math/abs (double x)) 5e-7) 0.0 x))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [S (Long/parseLong (nth toks 0))
            R (mapv (fn [j] (Double/parseDouble (nth toks (+ 1 (* j 4))))) (range S))
            th (mapv (fn [j] (Double/parseDouble (nth toks (+ 2 (* j 4))))) (range S))
            nd (mapv (fn [j] (Double/parseDouble (nth toks (+ 3 (* j 4))))) (range S))
            dn (mapv (fn [j] (Double/parseDouble (nth toks (+ 4 (* j 4))))) (range S))
            q (+ 1 (* S 4))
            h0 (Double/parseDouble (nth toks q)) u0 (Double/parseDouble (nth toks (+ q 1)))
            hb0 (Double/parseDouble (nth toks (+ q 2))) ub0 (Double/parseDouble (nth toks (+ q 3)))
            H (- (* ub0 h0) (* u0 hb0))
            res (loop [j 0 h h0 u u0 hb hb0 ub ub0 n 1.0 d 0.0
                       s1 0.0 s2 0.0 s3 0.0 s4 0.0 s5 0.0 c1 0.0 c2 0.0]
                  (if (>= j S) [s1 s2 s3 s4 s5 c1 c2]
                    (let [c (if (= (nth R j) 0.0) 0.0 (/ 1.0 (nth R j)))
                          np (nth nd j) dp (nth dn j)
                          up (/ (- (* n u) (* h c (- np n))) np)
                          ubp (/ (- (* n ub) (* hb c (- np n))) np)
                          A (* n (+ (* h c) u)) Ab (* n (+ (* hb c) ub))
                          dun (- (/ up np) (/ u n)) dinv (- (/ 1.0 np) (/ 1.0 n)) ddn (- (/ dp np) (/ d n))]
                      (recur (inc j) (+ h (* up (nth th j))) up (+ hb (* ubp (nth th j))) ubp np dp
                             (+ s1 (- (* (* A A) h dun))) (+ s2 (- (* (* A Ab) h dun)))
                             (+ s3 (- (* (* Ab Ab) h dun))) (+ s4 (- (* (* H H) c dinv)))
                             (+ s5 (if (> (Math/abs A) 1e-12)
                                     (- (* (/ Ab A) (+ (* (* Ab Ab) h dun) (* (* H H) c dinv)))) 0.0))
                             (+ c1 (* (* h A) ddn)) (+ c2 (* (* h Ab) ddn))))))]
        (println (fmt "SEIDEL: %.6f %.6f %.6f %.6f %.6f"
                      (sn (+ (nth res 0) 0.0)) (sn (+ (nth res 1) 0.0)) (sn (+ (nth res 2) 0.0))
                      (sn (+ (nth res 3) 0.0)) (sn (+ (nth res 4) 0.0))))
        (println (fmt "CHROM: %.6f %.6f" (sn (+ (nth res 5) 0.0)) (sn (+ (nth res 6) 0.0))))))))
(-main (slurp *in*))
