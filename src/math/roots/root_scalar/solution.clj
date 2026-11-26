;; Scalar root finding (NR 9.1-9.4).
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

(defn mkfunc [fname fp]
  [(fn [t]
     (cond
       (= fname "POLY") (loop [i (dec (count fp)) s 0.0]
                       (if (< i 0) s (recur (dec i) (+ (* s t) (nth fp i)))))
       (= fname "EXP") (+ (* (nth fp 0) (Math/exp (* (nth fp 1) t))) (nth fp 2))
       (= fname "TRIG") (* (nth fp 0) (Math/sin (+ (* (nth fp 1) t) (nth fp 2))))
       (= fname "LOG") (* (nth fp 0) (Math/log (+ (* (nth fp 1) t) (nth fp 2))))
       :else (* (nth fp 0) (Math/pow (+ (* (nth fp 1) t) (nth fp 2)) (nth fp 3)))))
   (fn [t]
     (cond
       (= fname "POLY") (loop [i (dec (count fp)) s 0.0]
                       (if (< i 1) s (recur (dec i) (+ (* s t) (* (double i) (nth fp i))))))
       (= fname "EXP") (* (* (nth fp 0) (nth fp 1)) (Math/exp (* (nth fp 1) t)))
       (= fname "TRIG") (* (* (nth fp 0) (nth fp 1)) (Math/cos (+ (* (nth fp 1) t) (nth fp 2))))
       (= fname "LOG") (/ (* (nth fp 0) (nth fp 1)) (+ (* (nth fp 1) t) (nth fp 2)))
       :else (* (* (* (nth fp 0) (nth fp 3)) (nth fp 1))
                (Math/pow (+ (* (nth fp 1) t) (nth fp 2)) (- (nth fp 3) 1.0)))))])
(defn rtbis [f x1 x2 xacc]
  (let [fv (f x1) fmid0 (f x2)]
    (if (>= (* fv fmid0) 0.0)
      [0.0 1]
      (let [[dx rtb] (if (< fv 0.0) [(- x2 x1) x1] [(- x1 x2) x2])]
        (loop [j 0 dx dx rtb rtb]
          (if (>= j 50)
            [rtb 1]
            (let [dx (* dx 0.5)
                  xmid (+ rtb dx)
                  fmid (f xmid)
                  rtb (if (<= fmid 0.0) xmid rtb)]
              (if (or (< (absv dx) xacc) (= fmid 0.0))
                [rtb 0]
                (recur (inc j) dx rtb)))))))))

(defn rtflsp [f x1 x2 xacc]
  (let [fl (f x1) fh (f x2)]
    (if (> (* fl fh) 0.0)
      [0.0 1]
      (let [[xl xh fl fh] (if (< fl 0.0) [x1 x2 fl fh] [x2 x1 fh fl])]
        (loop [j 0 xl xl xh xh fl fl fh fh dx (- xh xl) rtf xl]
          (if (>= j 30)
            [rtf 1]
            (let [rtf (+ xl (/ (* dx fl) (- fl fh)))
                  fv (f rtf)
                  [del xl xh fl fh] (if (< fv 0.0)
                                      [(- xl rtf) rtf xh fv fh]
                                      [(- xh rtf) xl rtf fl fv])
                  dx (- xh xl)]
              (if (or (< (absv del) xacc) (= fv 0.0))
                [rtf 0]
                (recur (inc j) xl xh fl fh dx rtf)))))))))

(defn rtsec [f x1 x2 xacc]
  (let [fl (f x1) fv (f x2)
        [xl rts fl fv] (if (< (absv fl) (absv fv)) [x2 x1 fv fl] [x1 x2 fl fv])]
    (loop [j 0 xl xl rts rts fl fl fv fv]
      (if (>= j 30)
        [rts 1]
        (let [dx (/ (* (- xl rts) fv) (- fv fl))
              xl rts
              fl fv
              rts (+ rts dx)
              fv (f rts)]
          (if (or (< (absv dx) xacc) (= fv 0.0))
            [rts 0]
            (recur (inc j) xl rts fl fv)))))))

(defn signb [a b] (* (absv a) (if (>= b 0.0) 1.0 -1.0)))

(defn zriddr [f x1 x2 xacc]
  (let [fl (f x1) fh (f x2)]
    (if (not (or (and (> fl 0.0) (< fh 0.0)) (and (< fl 0.0) (> fh 0.0))))
      (cond (= fl 0.0) [x1 0]
            (= fh 0.0) [x2 0]
            :else [0.0 1])
      (loop [j 0 xl x1 xh x2 fl fl fh fh ans -9.99e99]
        (if (>= j 60)
          [ans 1]
          (let [xm (* 0.5 (+ xl xh))
                fm (f xm)
                s (Math/sqrt (- (* fm fm) (* fl fh)))]
            (if (= s 0.0)
              [ans 0]
              (let [xnew (+ xm (* (- xm xl) (/ (* (if (>= fl fh) 1.0 -1.0) fm) s)))]
                (if (<= (absv (- xnew ans)) xacc)
                  [ans 0]
                  (let [ans xnew
                        fnew (f ans)]
                    (if (= fnew 0.0)
                      [ans 0]
                      (let [[xl xh fl fh]
                            (if (not= (signb fm fnew) fm)
                              [xm ans fm fnew]
                              (let [s2 (signb fl fnew)]
                                (if (not= s2 fl)
                                  [xl ans fl fnew]
                                  [ans xh fnew fh])))]
                        (if (<= (absv (- xh xl)) xacc)
                          [ans 0]
                          (recur (inc j) xl xh fl fh ans))))))))))))))

(defn zbrent [f x1 x2 tol]
  (let [eps 2.220446049250313e-16
        fa (f x1) fb (f x2)]
    (if (or (and (> fa 0.0) (> fb 0.0)) (and (< fa 0.0) (< fb 0.0)))
      [0.0 1]
      (loop [iter 0 a x1 b x2 c x2 d 0.0 e 0.0 fa fa fb fb fc fb]
        (if (>= iter 100)
          [b 1]
          (let [[c fc e d] (if (or (and (> fb 0.0) (> fc 0.0)) (and (< fb 0.0) (< fc 0.0)))
                             [a fa (- b a) (- b a)]
                             [c fc e d])
                [a b c fa fb fc] (if (< (absv fc) (absv fb))
                                   [b c a fb fc fa]
                                   [a b c fa fb fc])
                tol1 (+ (* 2.0 eps (absv b)) (* 0.5 tol))
                xm (* 0.5 (- c b))]
            (if (or (<= (absv xm) tol1) (= fb 0.0))
              [b 0]
              (let [[d e] (if (and (>= (absv e) tol1) (> (absv fa) (absv fb)))
                            (let [s (/ fb fa)
                                  [p q] (if (= a c)
                                          [(* (* 2.0 xm) s) (- 1.0 s)]
                                          (let [q (/ fa fc)
                                                r (/ fb fc)]
                                            [(* s (- (* (* (* 2.0 xm) q) (- q r)) (* (- b a) (- r 1.0))))
                                             (* (* (- q 1.0) (- r 1.0)) (- s 1.0))]))
                                  q (if (> p 0.0) (- q) q)
                                  p (absv p)
                                  min1 (- (* (* 3.0 xm) q) (absv (* tol1 q)))
                                  min2 (absv (* e q))]
                              (if (< (* 2.0 p) (if (< min1 min2) min1 min2))
                                [(/ p q) d]
                                [xm xm]))
                            [xm xm])
                    a b
                    fa fb
                    b (if (> (absv d) tol1)
                        (+ b d)
                        (+ b (* (absv tol1) (if (>= xm 0.0) 1.0 -1.0))))]
                (recur (inc iter) a b c d e fa (f b) fc)))))))))

(defn rtnewt [f df x1 x2 xacc]
  (loop [j 0 rtn (* 0.5 (+ x1 x2))]
    (if (>= j 20)
      [rtn 1]
      (let [fv (f rtn)
            dv (df rtn)
            dx (/ fv dv)
            rtn (- rtn dx)]
        (if (< (* (- x1 rtn) (- rtn x2)) 0.0)
          [0.0 1]
          (if (< (absv dx) xacc)
            [rtn 0]
            (recur (inc j) rtn)))))))

(defn rtsafe [f df x1 x2 xacc]
  (let [fl (f x1) fh (f x2)]
    (if (or (and (> fl 0.0) (> fh 0.0)) (and (< fl 0.0) (< fh 0.0)))
      [0.0 1]
      (cond (= fl 0.0) [x1 0]
            (= fh 0.0) [x2 0]
            :else (let [[xl xh] (if (< fl 0.0) [x1 x2] [x2 x1])]
                    (loop [j 0 rts (* 0.5 (+ x1 x2)) dxold (absv (- x2 x1)) dx (absv (- x2 x1))
                           fv (f (* 0.5 (+ x1 x2))) dv (df (* 0.5 (+ x1 x2))) xl xl xh xh]
                      (if (>= j 100)
                        [rts 1]
                        (let [[dxold dx rts done]
                              (if (or (> (* (- (* (- rts xh) dv) fv) (- (* (- rts xl) dv) fv)) 0.0)
                                      (> (absv (* 2.0 fv)) (absv (* dxold dv))))
                                (let [ndx (* 0.5 (- xh xl))
                                      nrts (+ xl ndx)]
                                  [dx ndx nrts (= xl nrts)])
                                (let [ndx (/ fv dv)
                                      nrts (- rts ndx)]
                                  [dx ndx nrts (= rts nrts)]))]
                          (if done
                            [rts 0]
                            (if (< (absv dx) xacc)
                              [rts 0]
                              (let [fv (f rts)
                                    dv (df rts)
                                    [xl xh] (if (< fv 0.0) [rts xh] [xl rts])]
                                (recur (inc j) rts dxold dx fv dv xl xh))))))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          fname (nth toks 1)
          [fp p] (cond (= fname "POLY") (let [d (Long/parseLong (nth toks 2))]
                                       [(vec (map #(Double/parseDouble %) (subvec toks 3 (+ 4 d)))) (+ 3 d 1)])
                     (= fname "POW") [(vec (map #(Double/parseDouble %) (subvec toks 2 6))) 6]
                     :else [(vec (map #(Double/parseDouble %) (subvec toks 2 5))) 5])
          x1 (Double/parseDouble (nth toks p))
          x2 (Double/parseDouble (nth toks (+ p 1)))
          xacc (Double/parseDouble (nth toks (+ p 2)))
          [f df] (mkfunc fname fp)
          res (cond (= op "BISECT") (rtbis f x1 x2 xacc)
                        (= op "SECANT") (rtsec f x1 x2 xacc)
                        (= op "FALSEPOS") (rtflsp f x1 x2 xacc)
                        (= op "RIDDER") (zriddr f x1 x2 xacc)
                        (= op "BRENT") (zbrent f x1 x2 xacc)
                        (= op "NEWTON") (rtnewt f df x1 x2 xacc)
                        (= op "RTSAFE") (rtsafe f df x1 x2 xacc))]
      (when res
        (let [[ans flag] res]
          (if (not= flag 0)
            (println "0.000000 1")
            (println (str (fmt6 (canon ans)) " 0"))))))))
