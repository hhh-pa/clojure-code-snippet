;; Gaussian quadrature rules (NR 4.6). Port of the C++ reference.
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

(defn fmtxw [x w]
  (str (fmt6 (canon x)) " " (fmt6 (canon w))))

(def gamm-c [76.18009172947146 -86.50532032941677 24.01409824083091
             -1.231739572450155 0.1208650973866179e-2 -0.5395239384953e-5])

(defn gammln [xx]
  (let [tmp (- (+ xx 5.5) (* (+ xx 0.5) (Math/log (+ xx 5.5))))
        ser (loop [j 0 y xx ser 1.000000000190015]
              (if (>= j 6) ser
                  (let [y (inc y)] (recur (inc j) y (+ ser (/ (nth gamm-c j) y))))))]
    (+ (- tmp) (Math/log (/ (* 2.5066282746310005 ser) xx)))))

(defn leg-root [z n eps]
  (loop [z z]
    (let [pp (loop [j 0 p1 1.0 p2 0.0 pp 0.0]
               (if (>= j n)
                 (let [pp (/ (* n (- (* z p1) p2)) (- (* z z) 1.0))] [p1 pp])
                 (let [p3 p2 p2n p1]
                   (recur (inc j) (/ (- (* (+ (* 2.0 j) 1.0) z p2n) (* j p3)) (+ j 1)) p2n pp))))
          p1 (nth pp 0)
          dp (nth pp 1)
          z1 z
          zn (- z1 (/ p1 dp))]
      (if (<= (absv (- zn z1)) eps) [zn dp] (recur zn)))))

(defn op-legendre [x1 x2 n]
  (let [m (quot (+ n 1) 2)
        xm (* 0.5 (+ x2 x1))
        xl (* 0.5 (- x2 x1))]
    (loop [i 0 x (vec (repeat n 0.0)) w (vec (repeat n 0.0))]
      (if (>= i m) [x w]
          (let [z0 (Math/cos (/ (* 3.141592654 (+ i 0.75)) (+ n 0.5)))
                [z pp] (leg-root z0 n 1.0e-14)
                ww (/ (* 2.0 xl) (* (- 1.0 (* z z)) pp pp))]
            (recur (inc i) (assoc x i (- xm (* xl z)) (- n 1 i) (+ xm (* xl z)))
                   (assoc w i ww (- n 1 i) ww)))))))

(defn lag-root [z n alf eps]
  (loop [z z its 0]
    (if (>= its 10) [z 0.0 0.0]
        (let [[p1 p2] (loop [j 0 p1 1.0 p2 0.0]
                        (if (>= j n) [p1 p2]
                            (let [p3 p2
                                  p2n p1
                                  p1n (/ (- (* (+ (* 2.0 j) 1.0 alf (- z)) p2n) (* (+ j alf) p3)) (+ j 1))]
                              (recur (inc j) p1n p2n))))
              pp (/ (- (* n p1) (* (+ n alf) p2)) z)
              z1 z
              zn (- z1 (/ p1 pp))]
          (if (<= (absv (- zn z1)) eps) [zn pp p2] (recur zn (inc its)))))))

(defn op-laguerre [alf n]
  (loop [i 0 z 0.0 x (vec (repeat n 0.0)) w (vec (repeat n 0.0))]
    (if (>= i n) [x w]
        (let [z (cond (zero? i) (/ (* (+ 1.0 alf) (+ 3.0 (* 0.92 alf))) (+ 1.0 (* 2.4 n) (* 1.8 alf)))
                      (= i 1) (+ z (/ (+ 15.0 (* 6.25 alf)) (+ 1.0 (* 0.9 alf) (* 2.5 n))))
                      :else (let [ai (dec i)]
                              (+ z (/ (* (+ (/ (+ 1.0 (* 2.55 ai)) (* 1.9 ai))
                                             (/ (* 1.26 ai alf) (+ 1.0 (* 3.5 ai))))
                                          (- z (nth x (- i 2)))) (+ 1.0 (* 0.3 alf))))))
              [z pp p2] (lag-root z n alf 1.0e-14)
              ww (/ (- (Math/exp (- (gammln (+ alf n)) (gammln n)))) (* pp n p2))]
          (recur (inc i) z (assoc x i z) (assoc w i ww))))))

(defn her-root [z n eps]
  (let [pim4 0.7511255444649425]
    (loop [z z its 0]
      (if (>= its 10) [z 0.0]
          (let [[p1 p2] (loop [j 0 p1 pim4 p2 0.0]
                          (if (>= j n) [p1 p2]
                              (let [p3 p2
                                    p2n p1
                                    p1n (- (* z (Math/sqrt (/ 2.0 (+ j 1))) p2n)
                                           (* (Math/sqrt (/ (double j) (+ j 1))) p3))]
                                (recur (inc j) p1n p2n))))
                pp (* (Math/sqrt (* 2.0 n)) p2)
                z1 z
                zn (- z1 (/ p1 pp))]
            (if (<= (absv (- zn z1)) eps) [zn pp] (recur zn (inc its))))))))

(defn op-hermite [n]
  (let [m (quot (+ n 1) 2)]
    (loop [i 0 z 0.0 x (vec (repeat n 0.0)) w (vec (repeat n 0.0))]
      (if (>= i m) [x w]
          (let [z (cond (zero? i) (- (Math/sqrt (+ (* 2.0 n) 1)) (* 1.85575 (Math/pow (+ (* 2.0 n) 1) -0.16667)))
                        (= i 1) (- z (/ (* 1.14 (Math/pow n 0.426)) z))
                        (= i 2) (- (* 1.86 z) (* 0.86 (nth x 0)))
                        (= i 3) (- (* 1.91 z) (* 0.91 (nth x 1)))
                        :else (- (* 2.0 z) (nth x (- i 2))))
                [z pp] (her-root z n 1.0e-14)
                ww (/ 2.0 (* pp pp))]
            (recur (inc i) z (assoc x i z (- n 1 i) (- z)) (assoc w i ww (- n 1 i) ww)))))))

(defn jac-root [z n alf bet eps]
  (let [alfbet (+ alf bet)]
    (loop [z z its 1]
      (if (> its 10) [z 0.0 1.0 2.0]
          (let [temp0 (+ 2.0 alfbet)
                p10 (/ (+ (- alf bet) (* temp0 z)) 2.0)
                [p1 p2 temp] (loop [j 2 p1 p10 p2 1.0 temp temp0]
                               (if (> j n) [p1 p2 temp]
                                   (let [p3 p2
                                         p2n p1
                                         temp (+ (* 2.0 j) alfbet)
                                         a (* (* 2.0 j) (+ j alfbet) (- temp 2.0))
                                         b (* (- temp 1.0) (+ (- (* alf alf) (* bet bet)) (* temp (- temp 2.0) z)))
                                         c (* 2.0 (+ (- j 1) alf) (+ (- j 1) bet) temp)
                                         p1n (/ (- (* b p2n) (* c p3)) a)]
                                     (recur (inc j) p1n p2n temp))))
                pp (/ (+ (* n (- (- alf bet) (* temp z)) p1) (* 2.0 (+ n alf) (+ n bet) p2))
                      (* temp (- 1.0 (* z z))))
                z1 z
                zn (- z1 (/ p1 pp))]
            (if (<= (absv (- zn z1)) eps) [zn pp p2 temp] (recur zn (inc its))))))))

(defn op-jacobi [alf bet n]
  (loop [i 0 z 0.0 x (vec (repeat n 0.0)) w (vec (repeat n 0.0))]
    (if (>= i n) [x w]
        (let [z (cond
                  (zero? i) (let [an (/ alf n) bn (/ bet n)
                                      r1 (* (+ 1.0 alf) (+ (/ 2.78 (+ 4.0 (* n n))) (/ (* 0.768 an) n)))
                                      r2 (+ 1.0 (* 1.48 an) (* 0.96 bn) (* 0.452 an an) (* 0.83 an bn))]
                                  (- 1.0 (/ r1 r2)))
                  (= i 1) (let [r1 (/ (+ 4.1 alf) (* (+ 1.0 alf) (+ 1.0 (* 0.156 alf))))
                                r2 (+ 1.0 (/ (* 0.06 (- n 8.0) (+ 1.0 (* 0.12 alf))) n))
                                r3 (+ 1.0 (/ (* 0.012 bet (+ 1.0 (* 0.25 (absv alf)))) n))]
                            (- z (* (- 1.0 z) r1 r2 r3)))
                  (= i 2) (let [r1 (/ (+ 1.67 (* 0.28 alf)) (+ 1.0 (* 0.37 alf)))
                                r2 (+ 1.0 (/ (* 0.22 (- n 8.0)) n))
                                r3 (+ 1.0 (/ (* 8.0 bet) (* (+ 6.28 bet) n n)))]
                            (- z (* (- (nth x 0) z) r1 r2 r3)))
                  (= i (- n 2)) (let [r1 (/ (+ 1.0 (* 0.235 bet)) (+ 0.766 (* 0.119 bet)))
                                      r2 (/ 1.0 (+ 1.0 (/ (* 0.639 (- n 4.0)) (+ 1.0 (* 0.71 (- n 4.0))))))
                                      r3 (/ 1.0 (+ 1.0 (/ (* 20.0 alf) (* (+ 7.5 alf) n n))))]
                                  (+ z (* (- z (nth x (- n 4))) r1 r2 r3)))
                  (= i (- n 1)) (let [r1 (/ (+ 1.0 (* 0.37 bet)) (+ 1.67 (* 0.28 bet)))
                                      r2 (/ 1.0 (+ 1.0 (/ (* 0.22 (- n 8.0)) n)))
                                      r3 (/ 1.0 (+ 1.0 (/ (* 8.0 alf) (* (+ 6.28 alf) n n))))]
                                  (+ z (* (- z (nth x (- n 3))) r1 r2 r3)))
                  :else (+ (* 3.0 (nth x (dec i))) (- (* 3.0 (nth x (- i 2)))) (nth x (- i 3))))
              [z pp p2 temp] (jac-root z n alf bet 1.0e-14)
              ww (/ (* (Math/exp (- (+ (gammln (+ alf n)) (gammln (+ bet n)))
                                           (gammln (+ n 1.0)) (gammln (+ n alf bet 1.0)))) temp (Math/pow 2.0 (+ alf bet))) (* pp p2))]
          (recur (inc i) z (assoc x i z) (assoc w i ww))))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          [x w] (cond (= op "LEGENDRE") (let [x1 (Double/parseDouble (nth toks 1))
                                                  x2 (Double/parseDouble (nth toks 2))
                                                  n (Long/parseLong (nth toks 3))]
                                              (op-legendre x1 x2 n))
                      (= op "LAGUERRE") (let [alf (Double/parseDouble (nth toks 1))
                                                  n (Long/parseLong (nth toks 2))]
                                              (op-laguerre alf n))
                      (= op "HERMITE") (let [n (Long/parseLong (nth toks 1))]
                                         (op-hermite n))
                      :else (let [alf (Double/parseDouble (nth toks 1))
                                  bet (Double/parseDouble (nth toks 2))
                                  n (Long/parseLong (nth toks 3))]
                              (op-jacobi alf bet n)))]
      (doseq [i (range (count x))]
        (println (fmtxw (nth x i) (nth w i)))))))
