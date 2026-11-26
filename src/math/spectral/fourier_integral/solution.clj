;; Fourier integrals I = Int_a^b exp(iwt)h(t) dt via FFT with cubic
;; endpoint corrections (dftcor) and the illustrative driver (dftint)
;; with polynomial interpolation of the DFT spectrum.
;; Reference: Press et al., Numerical Recipes 3e, Section 13.10
;; (dftcor/dftint, vendoring Poly_interp::rawinterp from 3.2 and
;; realft+four1 from 12.2-12.3).
;; Documented decisions: M = 64, NDFT = 1024, MPOL = 6 are fixed
;; exactly as printed (the book calls them merely illustrative,
;; but the printed program is what is extracted); h(t) arrives as
;; M + 1 explicit samples, not a callback (memcof-style explicit
;; input precedent); Poly_interp(xpol, ., MPOL).interp(en) is exactly
;; rawinterp(0, en) because mm == n clamps locate/hunt to jlo = 0
;; (fresh object, so cor = 0 takes the locate path); dftcor's throw
;; (a >= b, th < 0, th > 3.1416) and rawinterp's den == 0 become
;; value flags; divisions stay divisions (t2/45.0 etc. are NOT
;; rewritten as multiplies).
;; No online-judge problem found -- fixtures are self-generated
;; (scipy/numpy oracle), no header link.
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

(defn four1 [^doubles data n isign]
  (if (or (< n 2) (not= (bit-and n (dec n)) 0))
    false
    (let [nn (bit-shift-left n 1)]
      (loop [i 1 j 1]
        (when (< i nn)
          (when (> j i)
            (let [t (aget ^doubles data (dec j))]
              (aset ^doubles data (dec j) (aget ^doubles data (dec i)))
              (aset ^doubles data (dec i) t))
            (let [t (aget ^doubles data j)]
              (aset ^doubles data j (aget ^doubles data i))
              (aset ^doubles data i t)))
          (let [[m j] (loop [m n j j]
                        (if (and (>= m 2) (> j m))
                          (recur (bit-shift-right m 1) (- j m))
                          [m j]))]
            (recur (+ i 2) (+ j m)))))
      (loop [mmax 2]
        (when (> nn mmax)
          (let [istep (bit-shift-left mmax 1)
                theta (* (double isign) (/ 6.28318530717959 (double mmax)))
                wtemp (Math/sin (* 0.5 theta))
                wpr (* (* -2.0 wtemp) wtemp)
                wpi (Math/sin theta)]
            (loop [m 1 wr 1.0 wi 0.0]
              (when (< m mmax)
                (loop [i m]
                  (when (<= i nn)
                    (let [j (+ i mmax)
                          tempr (- (* wr (aget ^doubles data (dec j)))
                                   (* wi (aget ^doubles data j)))
                          tempi (+ (* wr (aget ^doubles data j))
                                   (* wi (aget ^doubles data (dec j))))]
                      (aset ^doubles data (dec j)
                            (- (aget ^doubles data (dec i)) tempr))
                      (aset ^doubles data j
                            (- (aget ^doubles data i) tempi))
                      (aset ^doubles data (dec i)
                            (+ (aget ^doubles data (dec i)) tempr))
                      (aset ^doubles data i
                            (+ (aget ^doubles data i) tempi)))
                    (recur (+ i istep))))
                (let [wtemp wr]
                  (recur (+ m 2)
                         (+ (- (* wtemp wpr) (* wi wpi)) wr)
                         (+ (+ (* wi wpr) (* wtemp wpi)) wi)))))
            (recur istep))))
      true)))

(defn realft [^doubles data isign]
  (let [n (alength ^doubles data)]
    (if (or (< n 4) (not= (bit-and n (dec n)) 0))
      false
      (let [c1 0.5
            half (bit-shift-right n 1)
            c2 (if (= isign 1) -0.5 0.5)
            theta (* (if (= isign 1) 1.0 -1.0)
                     (/ 3.141592653589793238 (double half)))]
        (when (= isign 1)
          (four1 data half 1))
        (let [wtemp (Math/sin (* 0.5 theta))
              wpr (* (* -2.0 wtemp) wtemp)
              wpi (Math/sin theta)]
          (loop [i 1 wr (+ 1.0 wpr) wi wpi]
            (when (< i (bit-shift-right n 2))
              (let [i1 (+ i i)
                    i2 (+ 1 i1)
                    i3 (- n i1)
                    i4 (+ 1 i3)
                    h1r (* c1 (+ (aget ^doubles data i1) (aget ^doubles data i3)))
                    h1i (* c1 (- (aget ^doubles data i2) (aget ^doubles data i4)))
                    h2r (* (- c2) (+ (aget ^doubles data i2) (aget ^doubles data i4)))
                    h2i (* c2 (- (aget ^doubles data i1) (aget ^doubles data i3)))]
                (aset ^doubles data i1
                      (- (+ h1r (* wr h2r)) (* wi h2i)))
                (aset ^doubles data i2
                      (+ (+ h1i (* wr h2i)) (* wi h2r)))
                (aset ^doubles data i3
                      (+ (- h1r (* wr h2r)) (* wi h2i)))
                (aset ^doubles data i4
                      (+ (+ (- h1i) (* wr h2i)) (* wi h2r)))
                (let [wtemp wr]
                  (recur (inc i)
                         (+ (- (* wtemp wpr) (* wi wpi)) wr)
                         (+ (+ (* wi wpr) (* wtemp wpi)) wi)))))))
        (if (= isign 1)
          (let [h1r (aget ^doubles data 0)]
            (aset ^doubles data 0 (+ h1r (aget ^doubles data 1)))
            (aset ^doubles data 1 (- h1r (aget ^doubles data 1))))
          (let [h1r (aget ^doubles data 0)]
            (aset ^doubles data 0 (* c1 (+ h1r (aget ^doubles data 1))))
            (aset ^doubles data 1 (* c1 (- h1r (aget ^doubles data 1))))
            (four1 data half -1)))
        true))))

(defn poly-raw [^doubles xa ^doubles ya x]
  (let [mm 6
        c (double-array mm)
        d (double-array mm)
        [ns] (loop [i 0 ns 0 dif (absv (- x (aget ^doubles xa 0)))]
               (if (>= i mm)
                 [ns]
                 (let [dift (absv (- x (aget ^doubles xa i)))
                       [ns dif] (if (< dift dif) [i dift] [ns dif])]
                   (aset ^doubles c i (aget ^doubles ya i))
                   (aset ^doubles d i (aget ^doubles ya i))
                   (recur (inc i) ns dif))))]
    (loop [m 1 y (aget ^doubles ya ns) ns (dec ns)]
      (if (>= m mm)
        y
        (let [bad (loop [i 0]
                    (cond (>= i (- mm m)) false
                          :else
                          (let [ho (- (aget ^doubles xa i) x)
                                hp (- (aget ^doubles xa (+ i m)) x)
                                w (- (aget ^doubles c (inc i))
                                     (aget ^doubles d i))
                                den (- ho hp)]
                            (if (= den 0.0)
                              true
                              (let [den (/ w den)]
                                (aset ^doubles d i (* hp den))
                                (aset ^doubles c i (* ho den))
                                (recur (inc i)))))))]
          (if bad
            nil
            (if (< (* 2 (+ ns 1)) (- mm m))
              (recur (inc m) (+ y (aget ^doubles c (+ ns 1))) ns)
              (recur (inc m) (+ y (aget ^doubles d ns)) (dec ns)))))))))

(defn dftcor [w delta a b ^doubles endpts]
  (let [th (* w delta)]
    (when-not (or (>= a b) (< th 0.0) (> th 3.1416))
      (let [[a0r a1r a2r a3r a0i a1i a2i a3i corfac]
            (if (< (absv th) 5.0e-2)
              (let [t th
                    t2 (* t t)
                    t4 (* t2 t2)
                    t6 (* t4 t2)]
                [(- (+ (/ -2.0 3.0) (/ t2 45.0) (* (/ 103.0 15120.0) t4))
                    (* (/ 169.0 226800.0) t6))
                 (- (+ (- (/ 7.0 24.0) (* (/ 7.0 180.0) t2))
                       (* (/ 5.0 3456.0) t4))
                    (* (/ 7.0 259200.0) t6))
                 (+ (- (+ (/ -1.0 6.0) (/ t2 45.0)) (* (/ 5.0 6048.0) t4))
                    (/ t6 64800.0))
                 (- (+ (- (/ 1.0 24.0) (/ t2 180.0)) (* (/ 5.0 24192.0) t4))
                    (/ t6 259200.0))
                 (* t (+ (- (+ (/ 2.0 45.0) (* (/ 2.0 105.0) t2))
                             (* (/ 8.0 2835.0) t4))
                          (* (/ 86.0 467775.0) t6)))
                 (* t (- (+ (- (/ 7.0 72.0) (/ t2 168.0))
                             (* (/ 11.0 72576.0) t4))
                          (* (/ 13.0 5987520.0) t6)))
                 (* t (+ (- (+ (/ -7.0 90.0) (/ t2 210.0))
                             (* (/ 11.0 90720.0) t4))
                          (* (/ 13.0 7484400.0) t6)))
                 (* t (- (+ (- (/ 7.0 360.0) (/ t2 840.0))
                             (* (/ 11.0 362880.0) t4))
                          (* (/ 13.0 29937600.0) t6)))
                 (+ (- 1.0 (* (/ 11.0 720.0) t4)) (* (/ 23.0 15120.0) t6))])
              (let [cth (Math/cos th)
                    sth (Math/sin th)
                    ctth (- (* cth cth) (* sth sth))
                    stth (* (* 2.0 sth) cth)
                    th2 (* th th)
                    th4 (* th2 th2)
                    tmth2 (- 3.0 th2)
                    spth2 (+ 6.0 th2)
                    sth4i (/ 1.0 (* 6.0 th4))
                    tth4i (* 2.0 sth4i)]
                [(* sth4i (+ (+ -42.0 (* 5.0 th2))
                               (* spth2 (- (* 8.0 cth) ctth))))
                 (* sth4i (- (* 14.0 tmth2) (* (* 7.0 spth2) cth)))
                 (* tth4i (+ (* -4.0 tmth2) (* (* 2.0 spth2) cth)))
                 (* sth4i (- (* 2.0 tmth2) (* spth2 cth)))
                 (* sth4i (+ (* th (+ -12.0 (* 6.0 th2))) (* spth2 stth)))
                 (* sth4i (- (* 30.0 th) (* (* 5.0 spth2) sth)))
                 (* tth4i (+ (* -12.0 th) (* (* 2.0 spth2) sth)))
                 (* sth4i (- (* 6.0 th) (* spth2 sth)))
                 (* (* tth4i spth2) (+ (- 3.0 (* 4.0 cth)) ctth))]))
            cl (+ (* a0r (aget ^doubles endpts 0))
                  (* a1r (aget ^doubles endpts 1))
                  (* a2r (aget ^doubles endpts 2))
                  (* a3r (aget ^doubles endpts 3)))
            sl (+ (* a0i (aget ^doubles endpts 0))
                  (* a1i (aget ^doubles endpts 1))
                  (* a2i (aget ^doubles endpts 2))
                  (* a3i (aget ^doubles endpts 3)))
            cr (+ (* a0r (aget ^doubles endpts 7))
                  (* a1r (aget ^doubles endpts 6))
                  (* a2r (aget ^doubles endpts 5))
                  (* a3r (aget ^doubles endpts 4)))
            sr (- (* (- a0i) (aget ^doubles endpts 7))
                  (* a1i (aget ^doubles endpts 6))
                  (* a2i (aget ^doubles endpts 5))
                  (* a3i (aget ^doubles endpts 4)))
            arg (* w (- b a))
            c (Math/cos arg)
            s (Math/sin arg)]
        [(- (+ cl (* c cr)) (* s sr))
         (+ (+ sl (* s cr)) (* c sr))
         corfac]))))

(defn dftint [a b w ^doubles h]
  (let [m 64 ndft 1024 mpol 6 twopi 6.283185307179586476]
    (when (= (alength ^doubles h) (inc m))
      (let [delta (/ (- b a) (double m))
            data (double-array ndft)
            endpts (double-array 8)]
        (dotimes [j (inc m)]
          (aset ^doubles data j (aget ^doubles h j)))
        (dotimes [j 4]
          (aset ^doubles endpts j (aget ^doubles data j))
          (aset ^doubles endpts (+ j 4) (aget ^doubles data (+ (- m 3) j))))
        (when (realft data 1)
          (aset ^doubles data 1 0.0)
          (let [en (+ (/ (* (* w delta) (double ndft)) twopi) 1.0)
                nn0 (long (+ (- en (* 0.5 (double mpol))) 1.0))
                nn (min (max nn0 1) (+ (- (quot ndft 2) mpol) 1))
                cpol (double-array mpol)
                spol (double-array mpol)
                xpol (double-array mpol)]
            (loop [j 0 nn nn]
              (when (< j mpol)
                (aset ^doubles cpol j (aget ^doubles data (- (* 2 nn) 2)))
                (aset ^doubles spol j (aget ^doubles data (- (* 2 nn) 1)))
                (aset ^doubles xpol j (double nn))
                (recur (inc j) (inc nn))))
            (let [cdft (poly-raw xpol cpol en)
                  sdft (poly-raw xpol spol en)]
              (when (and cdft sdft)
                (let [co (dftcor w delta a b endpts)]
                  (when co
                    (let [[corre corim corfac] co
                          cdft (+ (* cdft corfac) corre)
                          sdft (+ (* sdft corfac) corim)
                          c (* delta (Math/cos (* w a)))
                          s (* delta (Math/sin (* w a)))]
                      [(- (* c cdft) (* s sdft))
                       (+ (* s cdft) (* c sdft))])))))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (cond
        (= op "DFTCOR")
        (let [w (Double/parseDouble (nth toks 1))
              delta (Double/parseDouble (nth toks 2))
              a (Double/parseDouble (nth toks 3))
              b (Double/parseDouble (nth toks 4))
              endpts (double-array (map #(Double/parseDouble %)
                                        (subvec toks 5 13)))
              r (dftcor w delta a b endpts)]
          (if r
            (println (str (fmt6 (canon (nth r 0))) " "
                          (fmt6 (canon (nth r 1))) " "
                          (fmt6 (canon (nth r 2))) " 0"))
            (println "0.000000 0.000000 0.000000 1")))
        (= op "DFTINT")
        (let [a (Double/parseDouble (nth toks 1))
              b (Double/parseDouble (nth toks 2))
              w (Double/parseDouble (nth toks 3))
              h (double-array (map #(Double/parseDouble %)
                                   (subvec toks 4 69)))
              r (dftint a b w h)]
          (if r
            (println (str (fmt6 (canon (nth r 0))) " "
                          (fmt6 (canon (nth r 1))) " 0"))
            (println "0.000000 0.000000 1")))))))
