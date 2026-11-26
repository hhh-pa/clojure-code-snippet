;; One-dimensional minimization (NR 10.1-10.4): bracketing
;; (mnbrak), golden section search, Brent's method, and
;; derivative-using dBrent.
;; Reference: Press et al., Numerical Recipes 3e, Sections 10.1-10.4.
;; No online-judge problem found -- fixtures are self-generated
;; (scipy oracle), no header link.
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

(defn dmax [a b] (if (> a b) a b))

(defn sgn [a b] (if (>= b 0.0) (absv a) (- (absv a))))

(defn mkfunc [fname fp]
  [(fn [t]
     (cond (= fname "POLY") (loop [i (dec (count fp)) s 0.0]
                              (if (< i 0) s (recur (dec i) (+ (* s t) (nth fp i)))))
           (= fname "TRIG") (* (nth fp 0) (Math/sin (+ (* (nth fp 1) t) (nth fp 2))))
           :else (* (nth fp 0) (Math/pow (+ (* (nth fp 1) t) (nth fp 2)) (nth fp 3)))))
   (fn [t]
     (cond (= fname "POLY") (loop [i (dec (count fp)) s 0.0]
                              (if (< i 1) s (recur (dec i) (+ (* s t) (* (double i) (nth fp i))))))
           (= fname "TRIG") (* (* (nth fp 0) (nth fp 1))
                               (Math/cos (+ (* (nth fp 1) t) (nth fp 2))))
           :else (* (* (* (nth fp 0) (nth fp 3)) (nth fp 1))
                    (Math/pow (+ (* (nth fp 1) t) (nth fp 2)) (- (nth fp 3) 1.0)))))])
(defn bracket [f a b]
  (let [gold 1.618034
        glimit 100.0
        tiny 1.0e-20
        fa0 (f a)
        fb0 (f b)
        sw (> fb0 fa0)
        ax0 (if sw b a)
        bx0 (if sw a b)
        fa1 (if sw fb0 fa0)
        fb1 (if sw fa0 fb0)
        cx0 (+ bx0 (* gold (- bx0 ax0)))
        fc0 (f cx0)]
    (loop [ax ax0 bx bx0 cx cx0 fa fa1 fb fb1 fc fc0]
      (if (not (> fb fc))
        [ax bx cx fa fb fc]
        (let [r (* (- bx ax) (- fb fc))
              q (* (- bx cx) (- fb fa))
              u (- bx (/ (- (* (- bx cx) q) (* (- bx ax) r))
                         (* 2.0 (sgn (dmax (absv (- q r)) tiny) (- q r)))))
              ulim (+ bx (* glimit (- cx bx)))]
          (cond (> (* (- bx u) (- u cx)) 0.0)
                (let [fu (f u)]
                  (cond (< fu fc) [bx u cx fb fu fc]
                        (> fu fb) [ax bx u fa fb fu]
                        :else (let [u2 (+ cx (* gold (- cx bx)))
                                    fu2 (f u2)]
                                (recur bx cx u2 fb fc fu2))))
                (> (* (- cx u) (- u ulim)) 0.0)
                (let [fu (f u)]
                  (if (< fu fc)
                    (let [nu (+ u (* gold (- u cx)))
                          nfu (f nu)]
                      (recur cx u nu fc fu nfu))
                    (recur bx cx u fb fc fu)))
                (>= (* (- u ulim) (- ulim cx)) 0.0)
                (let [fu (f ulim)]
                  (recur bx cx ulim fb fc fu))
                :else
                (let [u2 (+ cx (* gold (- cx bx)))
                      fu2 (f u2)]
                  (recur bx cx u2 fb fc fu2))))))))

(defn golden [f br tol]
  (let [r 0.61803399
        c (- 1.0 r)
        ax (nth br 0)
        bx (nth br 1)
        cx (nth br 2)
        gt (> (absv (- cx bx)) (absv (- bx ax)))
        x1 (if gt bx (- bx (* c (- bx ax))))
        x2 (if gt (+ bx (* c (- cx bx))) bx)]
    (loop [x0 ax x1 x1 x2 x2 x3 cx f1 (f x1) f2 (f x2)]
      (if (not (> (absv (- x3 x0)) (* tol (+ (absv x1) (absv x2)))))
        (if (< f1 f2) [x1 f1] [x2 f2])
        (if (< f2 f1)
          (let [nx (+ (* r x2) (* c x3))]
            (recur x1 x2 nx x3 f2 (f nx)))
          (let [nx (+ (* r x1) (* c x0))]
            (recur x0 nx x1 x2 (f nx) f1)))))))

(defn brent [f br tol]
  (let [itmax 100
        cgold 0.3819660
        zeps (* 2.220446049250313e-16 1.0e-3)
        ax (nth br 0)
        bx (nth br 1)
        cx (nth br 2)
        a (if (< ax cx) ax cx)
        b (if (> ax cx) ax cx)
        fx0 (f bx)]
    (loop [its 0 a a b b d 0.0 e 0.0 v bx w bx x bx fv fx0 fw fx0 fx fx0]
      (if (>= its itmax)
        [x fx 1]
        (let [xm (* 0.5 (+ a b))
              tol1 (+ (* tol (absv x)) zeps)
              tol2 (* 2.0 tol1)]
          (if (<= (absv (- x xm)) (- tol2 (* 0.5 (- b a))))
            [x fx 0]
            (let [[nd ne] (if (> (absv e) tol1)
                            (let [r (* (- x w) (- fx fv))
                                  q0 (* (- x v) (- fx fw))
                                  p0 (- (* (- x v) q0) (* (- x w) r))
                                  q1 (* 2.0 (- q0 r))
                                  p (if (> q1 0.0) (- p0) p0)
                                  q (absv q1)
                                  etemp e]
                              (if (or (>= (absv p) (absv (* (* 0.5 q) etemp)))
                                      (<= p (* q (- a x)))
                                      (>= p (* q (- b x))))
                                (let [ne2 (if (>= x xm) (- a x) (- b x))]
                                  [(* cgold ne2) ne2])
                                (let [nd2 (/ p q)
                                      u (+ x nd2)]
                                  (if (or (< (- u a) tol2) (< (- b u) tol2))
                                    [(sgn tol1 (- xm x)) d]
                                    [nd2 d]))))
                            (let [ne2 (if (>= x xm) (- a x) (- b x))]
                              [(* cgold ne2) ne2]))
                  u (if (>= (absv nd) tol1) (+ x nd) (+ x (sgn tol1 nd)))
                  fu (f u)]
              (if (<= fu fx)
                (let [na (if (>= u x) x a)
                      nb (if (>= u x) b x)]
                  (recur (inc its) na nb nd ne w x u fw fx fu))
                (let [na (if (< u x) u a)
                      nb (if (< u x) b u)]
                  (cond (or (<= fu fw) (= w x))
                        (recur (inc its) na nb nd ne w u x fw fu fx)
                        (or (<= fu fv) (= v x) (= v w))
                        (recur (inc its) na nb nd ne u w x fu fw fx)
                        :else
                        (recur (inc its) na nb nd ne v w x fv fw fx)))))))))))

(defn dbrent [f df br tol]
  (let [itmax 100
        zeps (* 2.220446049250313e-16 1.0e-3)
        ax (nth br 0)
        bx (nth br 1)
        cx (nth br 2)
        a (if (< ax cx) ax cx)
        b (if (> ax cx) ax cx)
        fx0 (f bx)
        dx0 (df bx)]
    (loop [its 0 a a b b d 0.0 e 0.0 v bx w bx x bx
           fv fx0 fw fx0 fx fx0 dv dx0 dw dx0 dx dx0]
      (if (>= its itmax)
        [x fx 1]
        (let [xm (* 0.5 (+ a b))
              tol1 (+ (* tol (absv x)) zeps)
              tol2 (* 2.0 tol1)]
          (if (<= (absv (- x xm)) (- tol2 (* 0.5 (- b a))))
            [x fx 0]
            (let [[nd ne] (if (> (absv e) tol1)
                            (let [d1 (let [dd (* 2.0 (- b a))]
                                       (if (not= dw dx) (/ (* (- w x) dx) (- dx dw)) dd))
                                  d2 (let [dd (* 2.0 (- b a))]
                                       (if (not= dv dx) (/ (* (- v x) dx) (- dx dv)) dd))
                                  u1 (+ x d1)
                                  u2 (+ x d2)
                                  ok1 (and (> (* (- a u1) (- u1 b)) 0.0) (<= (* dx d1) 0.0))
                                  ok2 (and (> (* (- a u2) (- u2 b)) 0.0) (<= (* dx d2) 0.0))
                                  olde e]
                              (if (or ok1 ok2)
                                (let [dd (cond (and ok1 ok2) (if (< (absv d1) (absv d2)) d1 d2)
                                                 ok1 d1
                                                 :else d2)]
                                  (if (<= (absv dd) (absv (* 0.5 olde)))
                                    (let [u (+ x dd)]
                                      (if (or (< (- u a) tol2) (< (- b u) tol2))
                                        [(sgn tol1 (- xm x)) d]
                                        [dd d]))
                                    (let [ne2 (if (>= dx 0.0) (- a x) (- b x))]
                                      [(* 0.5 ne2) ne2])))
                                (let [ne2 (if (>= dx 0.0) (- a x) (- b x))]
                                  [(* 0.5 ne2) ne2])))
                            (let [ne2 (if (>= dx 0.0) (- a x) (- b x))]
                              [(* 0.5 ne2) ne2]))
                  uv (if (>= (absv nd) tol1)
                       [(+ x nd) (f (+ x nd)) false]
                       (let [uu (+ x (sgn tol1 nd))
                             fuu (f uu)]
                         (if (> fuu fx) [x fx true] [uu fuu false])))
                  u (nth uv 0)
                  fu (nth uv 1)
                  done (nth uv 2)]
              (if done
                [x fx 0]
                (let [du (df u)]
                  (if (<= fu fx)
                    (let [na (if (>= u x) x a)
                          nb (if (>= u x) b x)]
                      (recur (inc its) na nb nd ne w x u fw fx fu dw dx du))
                    (let [na (if (< u x) u a)
                          nb (if (< u x) b u)]
                      (cond (or (<= fu fw) (= w x))
                            (recur (inc its) na nb nd ne w u x fw fu fx dw du dx)
                            (or (< fu fv) (= v x) (= v w))
                            (recur (inc its) na nb nd ne u w x fu fw fx du dw dx)
                            :else
                            (recur (inc its) na nb nd ne v w x fv fw fx dv dw dx)))))))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          fname (nth toks 1)
          fpp (cond (= fname "POLY") (let [d (Long/parseLong (nth toks 2))]
                                       [(vec (map #(Double/parseDouble %) (subvec toks 3 (+ 4 d))))
                                        (+ 4 d)])
                    (= fname "POW") [(vec (map #(Double/parseDouble %) (subvec toks 2 6))) 6]
                    :else [(vec (map #(Double/parseDouble %) (subvec toks 2 5))) 5])
          fp (nth fpp 0)
          p (nth fpp 1)
          a (Double/parseDouble (nth toks p))
          b (Double/parseDouble (nth toks (+ p 1)))
          fdf (mkfunc fname fp)
          f (nth fdf 0)
          df (nth fdf 1)
          res (cond (= op "BRACKET") (let [br (bracket f a b)]
                                       (str/join " " (map #(fmt6 (canon %)) br)))
                    (or (= op "GOLDEN") (= op "BRENT") (= op "DBRENT"))
                    (let [tol (Double/parseDouble (nth toks (+ p 2)))
                          br (bracket f a b)
                          r (cond (= op "GOLDEN") (let [g (golden f br tol)]
                                                   [(nth g 0) (nth g 1) 0])
                                  (= op "BRENT") (brent f br tol)
                                  :else (dbrent f df br tol))
                          xmin (nth r 0)
                          fmin (nth r 1)
                          flag (nth r 2)]
                      (if (not= flag 0)
                        "0.000000 0.000000 1"
                        (str (fmt6 (canon xmin)) " " (fmt6 (canon fmin)) " 0"))))]
      (when res (println res)))))
