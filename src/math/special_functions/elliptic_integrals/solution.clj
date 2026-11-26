;; Elliptic integrals and Jacobian elliptic functions (NR 6.12).
;; Including the documented sncndn /= to *= correction.
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

(defn fmax [a b] (if (> a b) a b))
(defn fmin [a b] (if (< a b) a b))

(defn ell-rf [x y z]
  (let [errtol 0.0025 third (/ 1.0 3.0) c1 (/ 1.0 24.0) c2 0.1 c3 (/ 3.0 44.0) c4 (/ 1.0 14.0)]
    (loop [xt x yt y zt z]
      (let [sx (Math/sqrt xt) sy (Math/sqrt yt) sz (Math/sqrt zt)
            al (+ (* sx (+ sy sz)) (* sy sz))
            xt (* 0.25 (+ xt al)) yt (* 0.25 (+ yt al)) zt (* 0.25 (+ zt al))
            av (* third (+ xt yt zt))
            dx (/ (- av xt) av) dy (/ (- av yt) av) dz (/ (- av zt) av)]
        (if (> (fmax (fmax (absv dx) (absv dy)) (absv dz)) errtol)
          (recur xt yt zt)
          (let [e2 (- (* dx dy) (* dz dz)) e3 (* (* dx dy) dz)]
            (/ (+ 1.0 (* (- (* c1 e2) c2 (* c3 e3)) e2) (* c4 e3)) (Math/sqrt av))))))))

(defn ell-rc [x y]
  (let [errtol 0.0012 third (/ 1.0 3.0) c1 0.3 c2 (/ 1.0 7.0) c3 0.375 c4 (/ 9.0 22.0)
        xt0 (if (> y 0.0) x (- x y)) yt0 (if (> y 0.0) y (- y))
        w (if (> y 0.0) 1.0 (/ (Math/sqrt x) (Math/sqrt xt0)))]
    (loop [xt xt0 yt yt0]
      (let [al (+ (* (* 2.0 (Math/sqrt xt)) (Math/sqrt yt)) yt)
            xt (* 0.25 (+ xt al)) yt (* 0.25 (+ yt al))
            av (* third (+ xt yt yt)) s (/ (- yt av) av)]
        (if (> (absv s) errtol)
          (recur xt yt)
          (/ (* w (+ 1.0 (* (* s s) (+ c1 (* s (+ c2 (* s (+ c3 (* s c4))))))))) (Math/sqrt av)))))))

(defn ell-rd [x y z]
  (let [errtol 0.0015 c1 (/ 3.0 14.0) c2 (/ 1.0 6.0) c3 (/ 9.0 22.0)
        c4 (/ 3.0 26.0) c5 (* 0.25 c3) c6 (* 1.5 c4)]
    (loop [xt x yt y zt z sm 0.0 fac 1.0]
      (let [sx (Math/sqrt xt) sy (Math/sqrt yt) sz (Math/sqrt zt)
            al (+ (* sx (+ sy sz)) (* sy sz))
            sm (+ sm (/ fac (* sz (+ zt al)))) fac (* 0.25 fac)
            xt (* 0.25 (+ xt al)) yt (* 0.25 (+ yt al)) zt (* 0.25 (+ zt al))
            av (* 0.2 (+ xt yt (* 3.0 zt)))
            dx (/ (- av xt) av) dy (/ (- av yt) av) dz (/ (- av zt) av)]
        (if (> (fmax (fmax (absv dx) (absv dy)) (absv dz)) errtol)
          (recur xt yt zt sm fac)
          (let [ea (* dx dy) eb (* dz dz) ec (- ea eb)
                ed (- ea (* 6.0 eb)) ee (+ (+ ed ec) ec)]
            (+ (* 3.0 sm)
               (/ (* fac (+ 1.0 (* ed (+ (- c1) (* c5 ed) (- (* (* c6 dz) ee))))
                               (* dz (+ (* c2 ee) (* dz (+ (* (- c3) ec) (* (* dz c4) ea)))))))
                  (* av (Math/sqrt av))))))))))

(defn ell-rj [x y z p]
  (let [errtol 0.0015 c1 (/ 3.0 14.0) c2 (/ 1.0 3.0) c3 (/ 3.0 22.0)
        c4 (/ 3.0 26.0) c5 (* 0.75 c3) c6 (* 1.5 c4) c7 (* 0.5 c2) c8 (+ c3 c3)
        pos (> p 0.0)
        xt0 (if pos x (fmin (fmin x y) z))
        zt0 (if pos z (fmax (fmax x y) z))
        yt0 (if pos y (- (+ (+ x y) z) xt0 zt0))
        a (if pos 0.0 (/ 1.0 (- yt0 p)))
        b (if pos 0.0 (* (* a (- zt0 yt0)) (- yt0 xt0)))
        pt0 (if pos p (+ yt0 b))
        rcx (if pos 0.0 (ell-rc (/ (* xt0 zt0) yt0) (/ (* p pt0) yt0)))]
    (loop [xt xt0 yt yt0 zt zt0 pt pt0 sm 0.0 fac 1.0]
      (let [sx (Math/sqrt xt) sy (Math/sqrt yt) sz (Math/sqrt zt)
            al (+ (* sx (+ sy sz)) (* sy sz))
            t1 (+ (* pt (+ (+ sx sy) sz)) (* (* sx sy) sz)) alpha (* t1 t1)
            t2 (+ pt al) beta (* (* pt t2) t2)
            sm (+ sm (* fac (ell-rc alpha beta))) fac (* 0.25 fac)
            xt (* 0.25 (+ xt al)) yt (* 0.25 (+ yt al)) zt (* 0.25 (+ zt al)) pt (* 0.25 (+ pt al))
            av (* 0.2 (+ (+ (+ xt yt) zt) pt pt))
            dx (/ (- av xt) av) dy (/ (- av yt) av)
            dz (/ (- av zt) av) dp (/ (- av pt) av)]
        (if (> (fmax (fmax (absv dx) (absv dy)) (fmax (absv dz) (absv dp))) errtol)
          (recur xt yt zt pt sm fac)
          (let [ea (+ (* dx (+ dy dz)) (* dy dz)) eb (* (* dx dy) dz) ec (* dp dp)
                ed (- ea (* 3.0 ec)) ee (+ eb (* (* 2.0 dp) (- ea ec)))
                ans (+ (* 3.0 sm)
                       (/ (* fac (+ 1.0 (* ed (+ (- c1) (* c5 ed) (- (* c6 ee))))
                                       (* eb (+ c7 (* dp (+ (- c8) (* dp c4)))))
                                       (* (* dp ea) (- c2 (* dp c3)))
                                       (- (* (* c2 dp) ec))))
                          (* av (Math/sqrt av))))]
            (if (<= p 0.0)
              (* a (+ (* b ans) (* 3.0 (- rcx (ell-rf xt yt zt)))))
              ans)))))))

(defn ell-f [phi ak]
  (let [s (Math/sin phi) c (Math/cos phi)]
    (* s (ell-rf (* c c) (* (- 1.0 (* s ak)) (+ 1.0 (* s ak))) 1.0))))

(defn ell-e [phi ak]
  (let [s (Math/sin phi) c (Math/cos phi) cc (* c c)
        q (* (- 1.0 (* s ak)) (+ 1.0 (* s ak))) t (* s ak)]
    (* s (- (ell-rf cc q 1.0) (/ (* (* t t) (ell-rd cc q 1.0)) 3.0)))))

(defn ell-pi [phi en ak]
  (let [s (Math/sin phi) c (Math/cos phi) enss (* (* en s) s) cc (* c c)
        q (* (- 1.0 (* s ak)) (+ 1.0 (* s ak)))]
    (* s (- (ell-rf cc q 1.0) (/ (* enss (ell-rj cc q 1.0 (+ 1.0 enss))) 3.0)))))

(defn ell-sn [uu emmc]
  (let [ca 1.0e-8]
    (if (= emmc 0.0)
      (let [cn (/ 1.0 (Math/cosh uu))] [(Math/tanh uu) cn cn])
      (let [bo (< emmc 0.0)
            d0 (if bo (- 1.0 emmc) 0.0)
            emc0 (if bo (* emmc (/ -1.0 d0)) emmc)
            d (if bo (Math/sqrt d0) 0.0)
            u0 (if bo (* uu d) uu)
            emv (vec (repeat 13 0.0)) env (vec (repeat 13 0.0))
            [l em en _emc _a c] (loop [i 0 em emv en env emc emc0 a 1.0]
                                (let [em (assoc em i a)
                                      e2 (Math/sqrt emc)
                                      en (assoc en i e2)
                                      c2 (* 0.5 (+ a e2))]
                                  (if (<= (absv (- a e2)) (* ca a))
                                    [i em en e2 a c2]
                                    (if (>= i 12)
                                      [i em en (* e2 a) c2 c2]
                                      (recur (inc i) em en (* e2 a) c2)))))
            u (* u0 c)
            sn (Math/sin u) cn (Math/cos u)
            [sn cn dn] (if (= sn 0.0)
                         [sn cn 1.0]
                         (let [a0 (/ cn sn) c0 (* c a0)
                               [_a1 c1 dn1] (loop [ii l a a0 c c0 dn 1.0]
                                               (if (< ii 0)
                                                 [a c dn]
                                                 (let [b (nth em ii)
                                                       a2 (* a c)
                                                       c2 (* c dn)
                                                       dn2 (/ (+ (nth en ii) a2) (+ b a2))]
                                                   (recur (dec ii) (/ c2 b) c2 dn2))))
                               a2 (/ 1.0 (Math/sqrt (+ (* c1 c1) 1.0)))
                               sn2 (if (>= sn 0.0) a2 (- a2))]
                           [sn2 (* c1 sn2) dn1]))]
        (if bo [(/ sn d) dn cn] [sn cn dn])))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          f (fn [i] (Double/parseDouble (nth toks i)))]
      (cond
        (= op "RF") (println (fmt6 (canon (ell-rf (f 1) (f 2) (f 3)))))
        (= op "RD") (println (fmt6 (canon (ell-rd (f 1) (f 2) (f 3)))))
        (= op "RJ") (println (fmt6 (canon (ell-rj (f 1) (f 2) (f 3) (f 4)))))
        (= op "RC") (println (fmt6 (canon (ell-rc (f 1) (f 2)))))
        (= op "ELLF") (println (fmt6 (canon (ell-f (f 1) (f 2)))))
        (= op "ELLE") (println (fmt6 (canon (ell-e (f 1) (f 2)))))
        (= op "ELLPI") (println (fmt6 (canon (ell-pi (f 1) (f 2) (f 3)))))
        (= op "SN") (let [[sn cn dn] (ell-sn (f 1) (f 2))]
                      (println (str (fmt6 (canon sn)) " " (fmt6 (canon cn)) " " (fmt6 (canon dn)))))))))
