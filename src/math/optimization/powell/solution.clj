;; Multidimensional minimization by Powell's direction set
;; method (with Brent line searches).
;; Reference: Press et al., Numerical Recipes 3e, Sections 10.6-10.7.
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

(defn dget [a i j] (aget ^doubles (nth a i) j))

(defn dset! [a i j v] (aset ^doubles (nth a i) j (double v)))

(defn mkfunc [fname fp]
  (if (= fname "QUAD")
    (fn [^doubles x]
      (let [n (alength ^doubles x)]
        (loop [i 0 s (nth fp n)]
          (if (>= i n) s
            (let [d (- (aget ^doubles x i) (nth fp i))]
              (recur (inc i) (+ s (* d d))))))))
    (fn [^doubles x]
      (let [n (alength ^doubles x)]
        (loop [i 0 s 0.0]
          (if (>= i (dec n)) s
            (let [a (- (aget ^doubles x (inc i))
                       (* (aget ^doubles x i) (aget ^doubles x i)))
                  b (- 1.0 (aget ^doubles x i))]
              (recur (inc i) (+ s (+ (* (* 100.0 a) a) (* b b)))))))))))

(defn bracket1d [f a b]
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
        [ax bx cx]
        (let [r (* (- bx ax) (- fb fc))
              q (* (- bx cx) (- fb fa))
              u (- bx (/ (- (* (- bx cx) q) (* (- bx ax) r))
                         (* 2.0 (sgn (dmax (absv (- q r)) tiny) (- q r)))))
              ulim (+ bx (* glimit (- cx bx)))]
          (cond (> (* (- bx u) (- u cx)) 0.0)
                (let [fu (f u)]
                  (cond (< fu fc) [bx u cx]
                        (> fu fb) [ax bx u]
                        :else (let [u2 (+ cx (* gold (- cx bx)))]
                                (recur bx cx u2 fb fc (f u2)))))
                (> (* (- cx u) (- u ulim)) 0.0)
                (let [fu (f u)]
                  (if (< fu fc)
                    (let [nu (+ u (* gold (- u cx)))]
                      (recur cx u nu fc fu (f nu)))
                    (recur bx cx u fb fc fu)))
                (>= (* (- u ulim) (- ulim cx)) 0.0)
                (recur bx cx ulim fb fc (f ulim))
                :else
                (let [u2 (+ cx (* gold (- cx bx)))]
                  (recur bx cx u2 fb fc (f u2)))))))))

(defn brent1d [f ax bx cx tol]
  (let [itmax 100
        cgold 0.3819660
        zeps (* 2.220446049250313e-16 1.0e-3)
        a (if (< ax cx) ax cx)
        b (if (> ax cx) ax cx)
        fx0 (f bx)]
    (loop [its 0 a a b b d 0.0 e 0.0 v bx w bx x bx fv fx0 fw fx0 fx fx0]
      (if (>= its itmax)
        [x fx false]
        (let [xm (* 0.5 (+ a b))
              tol1 (+ (* tol (absv x)) zeps)
              tol2 (* 2.0 tol1)]
          (if (<= (absv (- x xm)) (- tol2 (* 0.5 (- b a))))
            [x fx true]
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

(defn linmin [f ^doubles p ^doubles xi]
  (let [n (alength ^doubles p)
        pc (double-array (map #(aget ^doubles p %) (range n)))
        xc (double-array (map #(aget ^doubles xi %) (range n)))
        f1 (fn [x]
             (let [xt (double-array n)]
               (dotimes [j n]
                 (aset ^doubles xt j (+ (aget ^doubles pc j)
                                        (* x (aget ^doubles xc j)))))
               (f xt)))
        br (bracket1d f1 0.0 1.0)
        r (brent1d f1 (nth br 0) (nth br 1) (nth br 2) 3.0e-8)]
    (if (not (nth r 2))
      [0.0 false]
      (let [xmin (nth r 0)]
        (dotimes [j n]
          (aset ^doubles xi j (* (aget ^doubles xi j) xmin))
          (aset ^doubles p j (+ (aget ^doubles p j) (aget ^doubles xi j))))
        [(nth r 1) true]))))

(defn powell [f point ftol]
  (let [n (count point)
        itmax 200
        tiny 1.0e-25
        ximat (vec (map (fn [i] (let [r (double-array n)]
                                   (aset ^doubles r i 1.0)
                                   r))
                         (range n)))
        p (double-array point)
        pt (double-array point)
        ptt (double-array n)
        xi (double-array n)]
    (loop [it 0 fret (f p)]
      (let [fp fret
            sw (loop [i 0 fret fret del 0.0 ibig 0]
                 (if (>= i n)
                   [fret del ibig true]
                   (do (dotimes [j n] (aset ^doubles xi j (dget ximat j i)))
                       (let [fptt fret
                             lr (linmin f p xi)]
                         (if (not (nth lr 1))
                           [fret del ibig false]
                           (let [nf (nth lr 0)]
                             (if (> (- fptt nf) del)
                               (recur (inc i) nf (- fptt nf) (inc i))
                               (recur (inc i) nf del ibig))))))))
            fret2 (nth sw 0)
            del (nth sw 1)
            ibig (nth sw 2)
            ok (nth sw 3)]
        (if (not ok)
          [p fret2 it 1]
          (if (<= (* 2.0 (- fp fret2))
                  (+ (* ftol (+ (absv fp) (absv fret2))) tiny))
            [p fret2 it 0]
            (if (= it itmax)
              [p fret2 it 1]
              (do (dotimes [j n]
                    (aset ^doubles ptt j (- (* 2.0 (aget ^doubles p j))
                                            (aget ^doubles pt j)))
                    (aset ^doubles xi j (- (aget ^doubles p j)
                                           (aget ^doubles pt j)))
                    (aset ^doubles pt j (aget ^doubles p j)))
                  (let [fptt (f ptt)
                        d1 (- (- fp fret2) del)
                        sq1 (* d1 d1)
                        d2 (- fp fptt)
                        sq2 (* d2 d2)
                        t (- (* (* 2.0 (+ (- fp (* 2.0 fret2)) fptt)) sq1)
                             (* del sq2))]
                    (if (and (< fptt fp) (< t 0.0))
                      (let [lr (linmin f p xi)]
                        (if (nth lr 1)
                          (do (dotimes [j n]
                                (dset! ximat j (dec ibig) (dget ximat j (dec n)))
                                (dset! ximat j (dec n) (aget ^doubles xi j)))
                              (recur (inc it) (nth lr 0)))
                          [p fret2 it 1]))
                      (recur (inc it) fret2)))))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          fname (nth toks 1)
          n (Long/parseLong (nth toks 2))
          fpp (if (= fname "QUAD")
                [(vec (map #(Double/parseDouble %) (subvec toks 3 (+ 4 n)))) (+ 4 n)]
                [[] 3])
          fp (nth fpp 0)
          pp (+ (nth fpp 1) 0)
          point (vec (map #(Double/parseDouble %) (subvec toks pp (+ pp n))))
          ftol (Double/parseDouble (nth toks (+ pp n)))
          res (when (= op "POWELL")
                (let [f (mkfunc fname fp)
                      r (powell f point ftol)
                      pmin (nth r 0)
                      fret (nth r 1)
                      it (nth r 2)
                      flag (nth r 3)]
                  (if (not= flag 0)
                    (str (str/join " " (repeat n "0.000000")) " 0.000000 " it " 1")
                    (str (str/join " " (map #(fmt6 (canon (aget ^doubles pmin %))) (range n)))
                         " " (fmt6 (canon fret)) " " it " 0"))))]
      (when res (println res)))))
