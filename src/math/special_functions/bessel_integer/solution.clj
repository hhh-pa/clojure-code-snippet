;; Integer Bessel functions (NR 6.5).
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

(def euler 0.577215664901533)

(defn j0s [x]
  (let [q (/ (* x x) 4.0)]
    (loop [k 1 term 1.0 sum 1.0]
      (if (> k 40) sum
          (let [term (* term (/ (- q) (* k k)))]
            (recur (inc k) term (+ sum term)))))))

(defn j1s [x]
  (let [q (/ (* x x) 4.0)]
    (loop [k 1 term 1.0 sum 1.0]
      (if (> k 40) (* 0.5 x sum)
          (let [term (* term (/ (- q) (* k (+ k 1.0))))]
            (recur (inc k) term (+ sum term)))))))

(defn pqjy [x nu]
  (let [mu (* 4.0 nu nu)
        r (/ 1.0 (* 8.0 x))]
    (loop [k 1 c 1.0 pw 1.0 p 1.0 q 0.0]
      (if (> k 10) [p q]
          (let [c (* c (/ (- mu (* (- (* 2.0 k) 1.0) (- (* 2.0 k) 1.0))) k))
                pw (* pw r)]
            (if (even? k)
              (recur (inc k) c pw (+ p (* (if (even? (/ k 2)) 1.0 -1.0) (* c pw))) q)
              (recur (inc k) c pw p (+ q (* (if (even? (/ (- k 1) 2)) 1.0 -1.0) (* c pw))))))))))

(defn j0f [x]
  (let [ax (absv x)]
    (if (< ax 8.0) (j0s x)
        (let [[p q] (pqjy ax 0)
              chi (- ax (/ Math/PI 4.0))]
          (* (Math/sqrt (/ 2.0 (* Math/PI ax))) (- (* p (Math/cos chi)) (* q (Math/sin chi))))))))

(defn j1f [x]
  (let [ax (absv x)
        v (if (< ax 8.0) (j1s ax)
              (let [[p q] (pqjy ax 1)
                    chi (- ax (/ Math/PI 2.0) (/ Math/PI 4.0))]
                (* (Math/sqrt (/ 2.0 (* Math/PI ax))) (- (* p (Math/cos chi)) (* q (Math/sin chi))))))]
    (if (neg? x) (- v) v)))

(defn y0s [x]
  (let [q (/ (* x x) 4.0)
        s (loop [k 1 m 1.0 h 0.0 s 0.0 sgn 1.0]
            (if (> k 40) s
                (let [m (* m (/ q (* k k)))
                      h (+ h (/ 1.0 k))]
                  (recur (inc k) m h (+ s (* sgn h m)) (- sgn)))))]
    (* (/ 2.0 Math/PI) (+ (* (+ (Math/log (/ x 2.0)) euler) (j0s x)) s))))

(defn y1s [x]
  (let [s (loop [k 0 pw (/ x 2.0) h 0.0 s 0.0 sgn 1.0]
            (if (> k 40) s
                (let [hp1 (+ h (/ 1.0 (+ k 1.0)))]
                  (recur (inc k) (* pw (/ (/ (* x x) 4.0) (* (+ k 1.0) (+ k 2.0)))) hp1
                         (+ s (* sgn (+ h hp1) pw)) (- sgn)))))]
    (- (* (/ 2.0 Math/PI) (+ (Math/log (/ x 2.0)) euler) (j1s x))
       (/ 2.0 (* Math/PI x)) (/ s Math/PI))))

(defn y0f [x]
  (if (< x 8.0) (y0s x)
      (let [[p q] (pqjy x 0)
            chi (- x (/ Math/PI 4.0))]
        (* (Math/sqrt (/ 2.0 (* Math/PI x))) (+ (* p (Math/sin chi)) (* q (Math/cos chi)))))))

(defn y1f [x]
  (if (< x 8.0) (y1s x)
      (let [[p q] (pqjy x 1)
            chi (- x (/ Math/PI 2.0) (/ Math/PI 4.0))]
        (* (Math/sqrt (/ 2.0 (* Math/PI x))) (+ (* p (Math/sin chi)) (* q (Math/cos chi)))))))

(defn i0s [x]
  (let [q (/ (* x x) 4.0)]
    (loop [k 1 term 1.0 sum 1.0]
      (if (> k 150) sum
          (let [term (* term (/ q (* k k)))]
            (recur (inc k) term (+ sum term)))))))

(defn i1s [x]
  (let [q (/ (* x x) 4.0)]
    (loop [k 1 term 1.0 sum 1.0]
      (if (> k 150) (* 0.5 x sum)
          (let [term (* term (/ q (* k (+ k 1.0))))]
            (recur (inc k) term (+ sum term)))))))

(defn aser [x nu nterms alt]
  (let [mu (* 4.0 nu nu)
        r (/ 1.0 (* 8.0 x))]
    (loop [k 1 c 1.0 pw 1.0 s 1.0]
      (if (> k nterms) s
          (let [c (* c (/ (- mu (* (- (* 2.0 k) 1.0) (- (* 2.0 k) 1.0))) k))
                pw (* pw r)]
            (recur (inc k) c pw (+ s (* (if (and alt (odd? k)) -1.0 1.0) (* c pw)))))))))

(defn i0f [x]
  (let [ax (absv x)]
    (if (< ax 15.0) (i0s x)
        (* (/ (Math/exp ax) (Math/sqrt (* 2.0 Math/PI ax))) (aser ax 0 20 true)))))

(defn i1f [x]
  (let [ax (absv x)
        v (if (< ax 15.0) (i1s ax)
              (* (/ (Math/exp ax) (Math/sqrt (* 2.0 Math/PI ax))) (aser ax 1 20 true)))]
    (if (neg? x) (- v) v)))

(defn k0s [x]
  (let [q (/ (* x x) 4.0)
        s (loop [k 1 m 1.0 h 0.0 s 0.0]
            (if (> k 60) s
                (let [m (* m (/ q (* k k)))
                      h (+ h (/ 1.0 k))]
                  (recur (inc k) m h (+ s (* h m))))))]
    (+ (* (- (+ (Math/log (/ x 2.0)) euler)) (i0s x)) s)))

(defn k1s [x]
  (let [s (loop [k 0 pw (/ x 2.0) h 0.0 s 0.0]
            (if (> k 60) s
                (let [hp1 (+ h (/ 1.0 (+ k 1.0)))]
                  (recur (inc k) (* pw (/ (/ (* x x) 4.0) (* (+ k 1.0) (+ k 2.0)))) hp1
                         (+ s (* (+ h hp1) pw))))))]
    (+ (/ 1.0 x) (* (+ (Math/log (/ x 2.0)) euler) (i1s x)) (- (* 0.5 s)))))

(defn k0f [x]
  (if (< x 6.0) (k0s x)
      (* (Math/sqrt (/ Math/PI (* 2.0 x))) (Math/exp (- x)) (aser x 0 16 false))))

(defn k1f [x]
  (if (< x 6.0) (k1s x)
      (* (Math/sqrt (/ Math/PI (* 2.0 x))) (Math/exp (- x)) (aser x 1 16 false))))

(defn dbl-exp [x]
  (let [m (if (neg? x) (- x) x)]
    (cond (and (not (< m 0.0)) (not (>= m 0.0))) 1024
          (> m 1.7976931348623157e308) 1024
          (< m 2.2250738585072014e-308) -1023
          :else (loop [e 0 m m]
                  (cond (>= m 2.0) (recur (inc e) (/ m 2.0))
                        (< m 1.0) (recur (dec e) (* m 2.0))
                        :else e)))))

(defn frexp-k [x]
  (if (zero? x) -1000 (inc (dbl-exp x))))

(defn ldexp-m512 [x]
  (loop [x (double x) n 512]
    (if (zero? n) x (recur (/ x 2.0) (dec n)))))

(defn bjn [n x]
  (cond (zero? n) (j0f x)
        (= n 1) (j1f x)
        :else (let [ax (absv x)]
                (if (<= (* ax ax) (* 8.0 2.2250738585072014e-308)) 0.0
                    (let [ans (if (> ax n)
                                (let [tox (/ 2.0 ax)]
                                  (loop [j 1 bjm (j0f ax) bj (j1f ax)]
                                    (if (>= j n) bj
                                        (recur (inc j) bj (- (* j tox bj) bjm)))))
                                (let [tox (/ 2.0 ax)
                                      m (* 2 (quot (+ n (long (Math/sqrt (* 160.0 n)))) 2))]
                                  (loop [j m bjp 0.0 bj 1.0 sum 0.0 ans 0.0 jsum false]
                                    (if (<= j 0)
                                      (/ ans (- (* 2.0 sum) bj))
                                      (let [bjm (- (* j tox bj) bjp)
                                            bjp bj
                                            bj bjm
                                            [bj bjp ans sum] (if (> (frexp-k bj) 512)
                                                               [(ldexp-m512 bj) (ldexp-m512 bjp) (ldexp-m512 ans) (ldexp-m512 sum)]
                                                               [bj bjp ans sum])
                                            sum (if jsum (+ sum bj) sum)
                                            ans (if (= j n) bjp ans)]
                                        (recur (dec j) bjp bj sum ans (not jsum)))))))]
                      (if (and (neg? x) (odd? n)) (- ans) ans))))))

(defn byn [n x]
  (cond (zero? n) (y0f x)
        (= n 1) (y1f x)
        :else (let [tox (/ 2.0 x)]
                (loop [j 1 by (y1f x) bym (y0f x)]
                  (if (>= j n) by
                      (recur (inc j) (- (* j tox by) bym) by))))))

(defn binf [n x]
  (cond (zero? n) (i0f x)
        (= n 1) (i1f x)
        :else (if (<= (* x x) (* 8.0 2.2250738585072014e-308)) 0.0
                    (let [tox (/ 2.0 (absv x))]
                      (loop [j (* 2 (+ n (long (Math/sqrt (* 200.0 n))))) bip 0.0 bi 1.0 ans 0.0]
                        (if (<= j 0)
                          (let [ans (* ans (/ (i0f x) bi))]
                            (if (and (neg? x) (odd? n)) (- ans) ans))
                          (let [bim (+ bip (* j tox bi))
                                bip bi
                                bi bim
                                [bi bip ans] (if (> (frexp-k bi) 512)
                                               [(ldexp-m512 bi) (ldexp-m512 bip) (ldexp-m512 ans)]
                                               [bi bip ans])
                                ans (if (= j n) bip ans)]
                            (recur (dec j) bip bi ans))))))))

(defn bkn [n x]
  (cond (zero? n) (k0f x)
        (= n 1) (k1f x)
        :else (let [tox (/ 2.0 x)]
                (loop [j 1 bk (k1f x) bkm (k0f x)]
                  (if (>= j n) bk
                      (recur (inc j) (+ bkm (* j tox bk)) bk))))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          n (Long/parseLong (nth toks 1))
          x (Double/parseDouble (nth toks 2))
          ans (cond (= op "JN") (bjn n x)
                    (= op "YN") (byn n x)
                    (= op "IN") (binf n x)
                    :else (bkn n x))]
      (println (fmt6 (canon ans))))))
