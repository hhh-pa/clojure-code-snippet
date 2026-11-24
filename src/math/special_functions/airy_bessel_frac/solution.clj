;; Fractional Bessel, Airy, spherical Bessel (NR 6.6).
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

(defn gammafn [x]
  (loop [prod 1.0 t x]
    (if (> t 0.0) (/ (Math/exp (gammln t)) prod)
        (recur (* prod t) (+ t 1.0)))))

(defn jfrac [nu x]
  (let [h (/ x 2.0)
        t0 (/ (Math/pow h nu) (gammafn (+ nu 1.0)))
        q (* h h)]
    (loop [k 1 t t0 s t0]
      (if (> k 80) s
          (let [t (* t (/ (- q) (* k (+ k nu))))]
            (recur (inc k) t (+ s t)))))))

(defn yfrac [nu x]
  (/ (- (* (jfrac nu x) (Math/cos (* nu Math/PI))) (jfrac (- nu) x)) (Math/sin (* nu Math/PI))))

(defn ifrac [nu x]
  (let [h (/ x 2.0)
        t0 (/ (Math/pow h nu) (gammafn (+ nu 1.0)))
        q (* h h)]
    (loop [k 1 t t0 s t0]
      (if (> k 80) s
          (let [t (* t (/ q (* k (+ k nu))))]
            (recur (inc k) t (+ s t)))))))

(defn aserf [x nu nterms alt]
  (let [mu (* 4.0 nu nu)
        r (/ 1.0 (* 8.0 x))]
    (loop [k 1 c 1.0 pw 1.0 s 1.0]
      (if (> k nterms) s
          (let [c (* c (/ (- mu (* (- (* 2.0 k) 1.0) (- (* 2.0 k) 1.0))) k))
                pw (* pw r)]
            (recur (inc k) c pw (+ s (* (if (and alt (odd? k)) -1.0 1.0) (* c pw)))))))))

(defn kfrac [nu x]
  (if (< x 6.0)
    (/ (* Math/PI (- (ifrac (- nu) x) (ifrac nu x))) (* 2.0 (Math/sin (* nu Math/PI))))
    (* (Math/sqrt (/ Math/PI (* 2.0 x))) (Math/exp (- x)) (aserf x nu 16 false))))

(defn airy [x]
  (let [onovrt 0.577350269189626]
    (if (zero? x)
      (let [g23 (gammafn (/ 2.0 3.0))
            g13 (gammafn (/ 1.0 3.0))]
        [(/ 1.0 (* (Math/pow 3.0 (/ 2.0 3.0)) g23))
         (/ 1.0 (* (Math/pow 3.0 (/ 1.0 6.0)) g23))
         (/ -1.0 (* (Math/pow 3.0 (/ 1.0 3.0)) g13))
         (/ (Math/pow 3.0 (/ 1.0 6.0)) g13)])
      (let [ax (absv x)
            rootx (Math/sqrt ax)
            z (* (/ 2.0 3.0) ax rootx)]
        (if (pos? x)
          (let [io (ifrac (/ 1.0 3.0) z)
                ko (kfrac (/ 1.0 3.0) z)
                io2 (ifrac (/ 2.0 3.0) z)
                ko2 (kfrac (/ 2.0 3.0) z)]
            [(/ (* rootx onovrt ko) Math/PI)
             (* rootx (+ (/ ko Math/PI) (* 2.0 onovrt io)))
             (/ (* (- x) onovrt ko2) Math/PI)
             (* x (+ (/ ko2 Math/PI) (* 2.0 onovrt io2)))])
          (let [jo (jfrac (/ 1.0 3.0) z)
                yo (yfrac (/ 1.0 3.0) z)
                jo2 (jfrac (/ 2.0 3.0) z)
                yo2 (yfrac (/ 2.0 3.0) z)]
            [(* 0.5 rootx (- jo (* onovrt yo)))
             (* -0.5 rootx (+ yo (* onovrt jo)))
             (* 0.5 ax (+ jo2 (* onovrt yo2)))
             (* 0.5 ax (- (* onovrt jo2) yo2))]))))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          ans (cond (or (= op "JFRAC") (= op "YFRAC") (= op "IFRAC") (= op "KFRAC"))
                    (let [nu (Double/parseDouble (nth toks 1))
                          x (Double/parseDouble (nth toks 2))]
                      (cond (= op "JFRAC") (jfrac nu x)
                            (= op "YFRAC") (yfrac nu x)
                            (= op "IFRAC") (ifrac nu x)
                            :else (kfrac nu x)))
                    (or (= op "AI") (= op "BI") (= op "AIP") (= op "BIP"))
                    (let [[ai bi aip bip] (airy (Double/parseDouble (nth toks 1)))]
                      (cond (= op "AI") ai (= op "BI") bi (= op "AIP") aip :else bip))
                    :else (let [n (Long/parseLong (nth toks 1))
                                x (Double/parseDouble (nth toks 2))
                                rtpio2 1.253314137315500251
                                nu (+ n 0.5)
                                factor (/ rtpio2 (Math/sqrt x))
                                jo (jfrac nu x)
                                yo (yfrac nu x)
                                jo1 (jfrac (+ nu 1.0) x)
                                yo1 (yfrac (+ nu 1.0) x)
                                jpo (- (* (/ nu x) jo) jo1)
                                ypo (- (* (/ nu x) yo) yo1)
                                sj (* factor jo)
                                sy (* factor yo)]
                            (cond (= op "SPHJ") sj
                                  (= op "SPHY") sy
                                  (= op "SPHJP") (- (* factor jpo) (/ sj (* 2.0 x)))
                                  :else (- (* factor ypo) (/ sy (* 2.0 x))))))]
      (println (fmt6 (canon ans))))))
