;; Nonlinear systems (NR 9.6-9.7): Newton (mnewt/newt),
;; Broyden with QR updates, backtracking lnsrch;
;; LU (2.3) and QR (2.10) engines vendored.
;; Reference: Press et al., Numerical Recipes 3e, Sections 9.6-9.7.
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

(defn dmat [n] (vec (repeatedly n #(double-array n))))

(defn dget [a i j] (aget ^doubles (nth a i) j))

(defn dset! [a i j v] (aset ^doubles (nth a i) j (double v)))

(defn dcopy! [^doubles dst ^doubles src n]
  (dotimes [i n] (aset ^doubles dst i (aget ^doubles src i))))

(defn mcopy [a]
  (let [n (count a)]
    (vec (map (fn [row] (let [r (double-array n)] (dcopy! r row n) r)) a))))

(defn maxabs [^doubles v n]
  (loop [i 0 t 0.0]
    (if (>= i n) t (let [a (absv (aget ^doubles v i))]
                     (recur (inc i) (if (> a t) a t))))))

(defn vecfunc [sysid ^doubles x]
  (let [n (alength ^doubles x)
        f (double-array n)
        x0 (aget ^doubles x 0)
        x1 (aget ^doubles x 1)]
    (cond (= sysid "SYS1")
          (do (aset ^doubles f 0 (double (- (+ (* x0 x0) (* x1 x1)) 4.0)))
              (aset ^doubles f 1 (double (- (* x0 x1) 1.0))))
          (= sysid "SYS2")
          (do (aset ^doubles f 0 (double (- (- (* x0 x0) x1) 1.0)))
              (aset ^doubles f 1 (double (+ (- x0 (* x1 x1)) 1.0))))
          :else
          (let [x2 (aget ^doubles x 2)]
            (aset ^doubles f 0 (double (- (+ (+ (* x0 x0) (* x1 x1)) (* x2 x2)) 14.0)))
            (aset ^doubles f 1 (double (- (* (* x0 x1) x2) 6.0)))
            (aset ^doubles f 2 (double (- (+ (+ x0 x1) x2) 6.0)))))
    f))

(defn jacfunc [sysid ^doubles x]
  (let [n (alength ^doubles x)
        j (dmat n)
        x0 (aget ^doubles x 0)
        x1 (aget ^doubles x 1)]
    (cond (= sysid "SYS1")
          (do (dset! j 0 0 (* 2.0 x0))
              (dset! j 0 1 (* 2.0 x1))
              (dset! j 1 0 x1)
              (dset! j 1 1 x0))
          (= sysid "SYS2")
          (do (dset! j 0 0 (* 2.0 x0))
              (dset! j 0 1 -1.0)
              (dset! j 1 0 1.0)
              (dset! j 1 1 (* -2.0 x1)))
          :else
          (let [x2 (aget ^doubles x 2)]
            (dset! j 0 0 (* 2.0 x0))
            (dset! j 0 1 (* 2.0 x1))
            (dset! j 0 2 (* 2.0 x2))
            (dset! j 1 0 (* x1 x2))
            (dset! j 1 1 (* x0 x2))
            (dset! j 1 2 (* x0 x1))
            (dset! j 2 0 1.0)
            (dset! j 2 1 1.0)
            (dset! j 2 2 1.0)))
    j))

(defn new-lu [a]
  (let [n (count a)
        lu (mcopy a)
        indx (long-array n)
        vv (double-array n)
        tiny 1.0e-40]
    (loop [i 0]
      (if (>= i n)
        (loop [k 0]
          (if (>= k n)
            {:n n :lu lu :indx indx}
            (let [big (loop [i k imax k b 0.0]
                          (if (>= i n) [imax b]
                            (let [t (* (aget ^doubles vv i) (absv (dget lu i k)))]
                              (if (> t b) (recur (inc i) i t) (recur (inc i) imax b)))))
                  imax (nth big 0)]
              (when (not= k imax)
                (loop [j 0]
                  (when (< j n)
                    (let [t (dget lu imax j)]
                      (dset! lu imax j (dget lu k j))
                      (dset! lu k j t)
                      (recur (inc j)))))
                (aset ^doubles vv imax (aget ^doubles vv k)))
              (aset ^longs indx k imax)
              (when (= (dget lu k k) 0.0) (dset! lu k k tiny))
              (loop [i (inc k)]
                (when (< i n)
                  (let [t (/ (dget lu i k) (dget lu k k))]
                    (dset! lu i k t)
                    (loop [j (inc k)]
                      (when (< j n)
                        (dset! lu i j (- (dget lu i j) (* t (dget lu k j))))
                        (recur (inc j)))))
                  (recur (inc i))))
              (recur (inc k)))))
        (let [big (loop [j 0 b 0.0]
                      (if (>= j n) b
                        (let [t (absv (dget lu i j))]
                          (recur (inc j) (if (> t b) t b)))))]
          (if (= big 0.0)
            nil
            (do (aset ^doubles vv i (/ 1.0 big))
                (recur (inc i)))))))))

(defn lu-solve [lum b x]
  (let [n (:n lum) lu (:lu lum) indx (:indx lum)]
    (dcopy! x b n)
    (loop [i 0 ii 0]
      (when (< i n)
        (let [ip (aget ^longs indx i)
              sum (aget ^doubles x ip)]
          (aset ^doubles x ip (aget ^doubles x i))
          (let [s (if (not= ii 0)
                    (loop [j (dec ii) s sum]
                      (if (>= j i) s (recur (inc j) (- s (* (dget lu i j) (aget ^doubles x j))))))
                    sum)
                nii (if (not= ii 0) ii (if (not= sum 0.0) (inc i) ii))]
            (aset ^doubles x i (double s))
            (recur (inc i) nii)))))
    (loop [i (dec n)]
      (when (>= i 0)
        (let [s (loop [j (inc i) s (aget ^doubles x i)]
                    (if (>= j n) s (recur (inc j) (- s (* (dget lu i j) (aget ^doubles x j))))))]
          (aset ^doubles x i (double (/ s (dget lu i i))))
          (recur (dec i)))))))

(defn new-qr [a]
  (let [n (count a)
        qt (dmat n)
        r (mcopy a)
        c (double-array n)
        d (double-array n)
        sing (atom false)]
    (loop [k 0]
      (when (< k (dec n))
        (let [scale (loop [i k s 0.0]
                        (if (>= i n) s
                          (let [t (absv (dget r i k))]
                            (recur (inc i) (if (> t s) t s)))))]
          (if (= scale 0.0)
            (do (reset! sing true)
                (aset ^doubles c k 0.0)
                (aset ^doubles d k 0.0))
            (do (loop [i k]
                  (when (< i n)
                    (dset! r i k (/ (dget r i k) scale))
                    (recur (inc i))))
                (let [sum (loop [i k s 0.0]
                              (if (>= i n) s
                                (recur (inc i) (+ s (* (dget r i k) (dget r i k))))))
                      rk (dget r k k)
                      sigma (if (>= rk 0.0) (absv (Math/sqrt sum)) (- (absv (Math/sqrt sum))))]
                  (dset! r k k (+ rk sigma))
                  (aset ^doubles c k (* sigma (dget r k k)))
                  (aset ^doubles d k (* (- scale) sigma))
                  (loop [j (inc k)]
                    (when (< j n)
                      (let [s (loop [i k s 0.0]
                                  (if (>= i n) s
                                    (recur (inc i) (+ s (* (dget r i k) (dget r i j))))))
                            tau (/ s (aget ^doubles c k))]
                        (loop [i k]
                          (when (< i n)
                            (dset! r i j (- (dget r i j) (* tau (dget r i k))))
                            (recur (inc i)))))
                      (recur (inc j)))))))
          (recur (inc k)))))
    (aset ^doubles d (dec n) (dget r (dec n) (dec n)))
    (when (= (aget ^doubles d (dec n)) 0.0) (reset! sing true))
    (loop [i 0]
      (when (< i n)
        (loop [j 0]
          (when (< j n)
            (dset! qt i j (if (= i j) 1.0 0.0))
            (recur (inc j))))
        (recur (inc i))))
    (loop [k 0]
      (when (< k (dec n))
        (when (not= (aget ^doubles c k) 0.0)
          (loop [j 0]
            (when (< j n)
              (let [sum (/ (loop [i k s 0.0]
                                 (if (>= i n) s
                                   (recur (inc i) (+ s (* (dget r i k) (dget qt i j))))))
                             (aget ^doubles c k))]
                (loop [i k]
                  (when (< i n)
                    (dset! qt i j (- (dget qt i j) (* sum (dget r i k))))
                    (recur (inc i)))))
              (recur (inc j)))))
        (recur (inc k))))
    (loop [i 0]
      (when (< i n)
        (dset! r i i (aget ^doubles d i))
        (loop [j 0]
          (when (< j i)
            (dset! r i j 0.0)
            (recur (inc j))))
        (recur (inc i))))
    {:n n :qt qt :r r :c c :d d :sing @sing}))

(defn qr-rotate [qr i a b]
  (let [n (:n qr) r (:r qr) qt (:qt qr)
        cs (if (= a 0.0)
             [0.0 (if (>= b 0.0) 1.0 -1.0)]
             (if (> (absv a) (absv b))
               (let [fact (/ b a)
                     v (absv (/ 1.0 (Math/sqrt (+ 1.0 (* fact fact)))))
                     cc (if (>= a 0.0) v (- v))]
                 [cc (* fact cc)])
               (let [fact (/ a b)
                     v (absv (/ 1.0 (Math/sqrt (+ 1.0 (* fact fact)))))
                     s (if (>= b 0.0) v (- v))]
                 [(* fact s) s])))
        cc (nth cs 0) s (nth cs 1)]
    (loop [j i]
      (when (< j n)
        (let [y (dget r i j) w (dget r (inc i) j)]
          (dset! r i j (- (* cc y) (* s w)))
          (dset! r (inc i) j (+ (* s y) (* cc w))))
        (recur (inc j))))
    (loop [j 0]
      (when (< j n)
        (let [y (dget qt i j) w (dget qt (inc i) j)]
          (dset! qt i j (- (* cc y) (* s w)))
          (dset! qt (inc i) j (+ (* s y) (* cc w))))
        (recur (inc j))))))

(defn qr-qtmult [qr b x]
  (let [n (:n qr) qt (:qt qr)]
    (loop [i 0]
      (when (< i n)
        (aset ^doubles x i (double (loop [j 0 s 0.0]
                                      (if (>= j n) s
                                        (recur (inc j) (+ s (* (dget qt i j) (aget ^doubles b j))))))))
        (recur (inc i))))))

(defn qr-rsolve [qr b x]
  (let [n (:n qr) r (:r qr)]
    (dcopy! x b n)
    (loop [i (dec n)]
      (when (>= i 0)
        (aset ^doubles x i (double (/ (loop [j (inc i) s (aget ^doubles x i)]
                                         (if (>= j n) s
                                           (recur (inc j) (- s (* (dget r i j) (aget ^doubles x j))))))
                                       (dget r i i))))
        (recur (dec i))))))

(defn qr-update [qr u v]
  (let [n (:n qr) r (:r qr)
        w (double-array n)]
    (dcopy! w u n)
    (let [k (loop [kk (dec n)]
              (cond (< kk 0) 0
                    (not= (aget ^doubles w kk) 0.0) kk
                    :else (recur (dec kk))))]
      (loop [i (dec k)]
        (when (>= i 0)
          (qr-rotate qr i (aget ^doubles w i) (- (aget ^doubles w (inc i))))
          (let [wi (aget ^doubles w i)
                w1 (aget ^doubles w (inc i))]
            (cond (= wi 0.0) (aset ^doubles w i (absv w1))
                  (> (absv wi) (absv w1))
                  (let [t (/ w1 wi)]
                    (aset ^doubles w i (double (* (absv wi) (Math/sqrt (+ 1.0 (* t t)))))))
                  :else
                  (let [t (/ wi w1)]
                    (aset ^doubles w i (double (* (absv w1) (Math/sqrt (+ 1.0 (* t t)))))))))
          (recur (dec i))))
      (loop [i 0]
        (when (< i n)
          (dset! r 0 i (+ (dget r 0 i) (* (aget ^doubles w 0) (aget ^doubles v i))))
          (recur (inc i))))
      (loop [i 0]
        (when (< i k)
          (qr-rotate qr i (dget r i i) (- (dget r (inc i) i)))
          (recur (inc i))))
      (loop [i 0 sg false]
        (if (>= i n) sg (recur (inc i) (or sg (= (dget r i i) 0.0))))))))

(defn fdjac [sysid ^doubles x ^doubles fvec]
  (let [n (alength ^doubles x)
        df (dmat n)
        xh (double-array n)
        eps 1.0e-8]
    (dcopy! xh x n)
    (loop [j 0]
      (when (< j n)
        (let [temp (aget ^doubles xh j)
              h0 (* eps (absv temp))
              h1 (if (= h0 0.0) eps h0)]
          (aset ^doubles xh j (+ temp h1))
          (let [h (- (aget ^doubles xh j) temp)
                f (vecfunc sysid xh)]
            (aset ^doubles xh j temp)
            (loop [i 0]
              (when (< i n)
                (dset! df i j (/ (- (aget ^doubles f i) (aget ^doubles fvec i)) h))
                (recur (inc i))))))
        (recur (inc j))))
    df))

(defn fminfn [sysid ^doubles x ^doubles fvec]
  (let [f (vecfunc sysid x)
        n (alength ^doubles x)]
    (dcopy! fvec f n)
    (* 0.5 (loop [i 0 s 0.0]
               (if (>= i n) s
                 (let [v (aget ^doubles fvec i)]
                   (recur (inc i) (+ s (* v v)))))))))

(defn mnewt [sysid ^doubles x ntrial tolx tolf]
  (let [n (alength ^doubles x)
        p (double-array n)
        fvec (double-array n)]
    (loop [k 0]
      (if (>= k ntrial)
        false
        (do (dcopy! fvec (vecfunc sysid x) n)
            (let [fjac (jacfunc sysid x)
                  errf (loop [i 0 s 0.0]
                           (if (>= i n) s (recur (inc i) (+ s (absv (aget ^doubles fvec i))))))]
              (if (<= errf tolf)
                false
                (do (loop [i 0]
                      (when (< i n)
                        (aset ^doubles p i (- (aget ^doubles fvec i)))
                        (recur (inc i))))
                    (let [alu (new-lu fjac)]
                      (if (nil? alu)
                        true
                        (do (lu-solve alu p p)
                            (let [errx (loop [i 0 s 0.0]
                                           (if (>= i n) s
                                             (let [a (absv (aget ^doubles p i))]
                                               (aset ^doubles x i (+ (aget ^doubles x i) (aget ^doubles p i)))
                                               (recur (inc i) (+ s a)))))]
                              (if (<= errx tolx) false (recur (inc k)))))))))))))))

(defn lnsrch [sysid ^doubles xold fold ^doubles g ^doubles p ^doubles x stpmax]
  (let [n (alength ^doubles xold)
        alf 1.0e-4
        tolx 2.220446049250313e-16
        sum (loop [i 0 s 0.0]
                (if (>= i n) s
                  (let [v (aget ^doubles p i)] (recur (inc i) (+ s (* v v))))))
        sum (Math/sqrt sum)]
    (when (> sum stpmax)
      (let [r (/ stpmax sum)]
        (loop [i 0]
          (when (< i n)
            (aset ^doubles p i (* (aget ^doubles p i) r))
            (recur (inc i))))))
    (let [slope (loop [i 0 s 0.0]
                    (if (>= i n) s
                      (recur (inc i) (+ s (* (aget ^doubles g i) (aget ^doubles p i))))))]
      (if (>= slope 0.0)
        [0.0 false 1]
        (let [test (loop [i 0 t 0.0]
                       (if (>= i n) t
                         (let [v (/ (absv (aget ^doubles p i))
                                    (dmax (absv (aget ^doubles xold i)) 1.0))]
                           (recur (inc i) (if (> v t) v t)))))
              alamin (/ tolx test)
              fvec (double-array n)]
          (loop [alam 1.0 alam2 0.0 f2 0.0]
            (loop [i 0]
              (when (< i n)
                (aset ^doubles x i (+ (aget ^doubles xold i) (* alam (aget ^doubles p i))))
                (recur (inc i))))
            (let [f (fminfn sysid x fvec)]
              (cond (< alam alamin)
                    (do (dcopy! x xold n) [f true 0])
                    (<= f (+ fold (* (* alf alam) slope)))
                    [f false 0]
                    :else
                    (let [tmplam (if (= alam 1.0)
                                   (/ (- slope) (* 2.0 (- (- f fold) slope)))
                                   (let [raw (let [rhs1 (- (- f fold) (* alam slope))
                                                   rhs2 (- (- f2 fold) (* alam2 slope))
                                                   a (/ (- (/ rhs1 (* alam alam))
                                                            (/ rhs2 (* alam2 alam2)))
                                                          (- alam alam2))
                                                   b (/ (+ (/ (* (- alam2) rhs1) (* alam alam))
                                                             (/ (* alam rhs2) (* alam2 alam2)))
                                                          (- alam alam2))]
                                               (if (= a 0.0)
                                                 (/ (- slope) (* 2.0 b))
                                                 (let [disc (- (* b b) (* (* 3.0 a) slope))]
                                                   (cond (< disc 0.0) (* 0.5 alam)
                                                         (<= b 0.0) (/ (+ (- b) (Math/sqrt disc))
                                                                       (* 3.0 a))
                                                         :else (/ (- slope)
                                                                  (+ b (Math/sqrt disc)))))))]
                                     (if (> raw (* 0.5 alam)) (* 0.5 alam) raw)))]
                      (recur (dmax tmplam (* 0.1 alam)) alam f))))))))))

(defn newt [sysid ^doubles x]
  (let [n (alength ^doubles x)
        maxits 200
        tolf 1.0e-8
        tolmin 1.0e-12
        stpmx 100.0
        tolx 2.220446049250313e-16
        g (double-array n)
        p (double-array n)
        xold (double-array n)
        fvec (double-array n)
        f0 (fminfn sysid x fvec)
        test0 (maxabs fvec n)]
    (if (< test0 (* 0.01 tolf))
      [false 0]
      (let [ss (loop [i 0 s 0.0]
                   (if (>= i n) s
                     (let [v (aget ^doubles x i)] (recur (inc i) (+ s (* v v))))))
            stpmax (* stpmx (dmax (Math/sqrt ss) (double n)))]
        (loop [its 0 f f0 check false]
          (if (>= its maxits)
            [check 1]
            (let [fjac (fdjac sysid x fvec)]
              (loop [i 0]
                (when (< i n)
                  (aset ^doubles g i (double (loop [j 0 s 0.0]
                                                 (if (>= j n) s
                                                   (recur (inc j) (+ s (* (dget fjac j i)
                                                                          (aget ^doubles fvec j))))))))
                  (recur (inc i))))
              (dcopy! xold x n)
              (let [fold f]
                (loop [i 0]
                  (when (< i n)
                    (aset ^doubles p i (- (aget ^doubles fvec i)))
                    (recur (inc i))))
                (let [alu (new-lu fjac)]
                  (if (nil? alu)
                    [check 1]
                    (do (lu-solve alu p p)
                        (let [[nf ncheck bad] (lnsrch sysid xold fold g p x stpmax)]
                          (if (not= bad 0)
                            [ncheck 1]
                            (do (dcopy! fvec (vecfunc sysid x) n)
                                (let [test (maxabs fvec n)]
                                  (cond (< test tolf) [false 0]
                                        ncheck
                                        (let [den (dmax nf (* 0.5 (double n)))
                                              t2 (loop [i 0 t 0.0]
                                                     (if (>= i n) t
                                                       (let [v (/ (* (absv (aget ^doubles g i))
                                                                       (dmax (absv (aget ^doubles x i)) 1.0))
                                                                    den)]
                                                         (recur (inc i) (if (> v t) v t)))))]
                                          [(< t2 tolmin) 0])
                                        :else
                                        (let [t3 (loop [i 0 t 0.0]
                                                       (if (>= i n) t
                                                         (let [v (/ (absv (- (aget ^doubles x i)
                                                                              (aget ^doubles xold i)))
                                                                      (dmax (absv (aget ^doubles x i)) 1.0))]
                                                           (recur (inc i) (if (> v t) v t)))))]
                                          (if (< t3 tolx) [ncheck 0] (recur (inc its) nf ncheck)))))))))))))))))))

(defn broydn [sysid ^doubles x]
  (let [n (alength ^doubles x)
        maxits 200
        eps 2.220446049250313e-16
        tolf 1.0e-8
        tolx eps
        stpmx 100.0
        tolmin 1.0e-12
        fvcold (double-array n)
        g (double-array n)
        p (double-array n)
        s (double-array n)
        t (double-array n)
        w (double-array n)
        xold (double-array n)
        fvec (double-array n)
        f0 (fminfn sysid x fvec)
        test0 (maxabs fvec n)]
    (if (< test0 (* 0.01 tolf))
      [false 0]
      (let [ss (loop [i 0 s2 0.0]
                   (if (>= i n) s2
                     (let [v (aget ^doubles x i)] (recur (inc i) (+ s2 (* v v))))))
            stpmax (* stpmx (dmax (Math/sqrt ss) (double n)))]
        (loop [its 1 f f0 check false restrt true qr nil flag 0]
          (if (> its maxits)
            [check flag]
            (let [[qr2 failed]
                  (if restrt
                    (let [q (new-qr (fdjac sysid x fvec))] [q (:sing q)])
                    (do
                      (loop [i 0]
                        (when (< i n)
                          (aset ^doubles s i (- (aget ^doubles x i) (aget ^doubles xold i)))
                          (recur (inc i))))
                      (let [r (:r qr)]
                        (loop [i 0]
                          (when (< i n)
                            (aset ^doubles t i (double (loop [j i s2 0.0]
                                                           (if (>= j n) s2
                                                             (recur (inc j) (+ s2 (* (dget r i j)
                                                                                      (aget ^doubles s j))))))))
                            (recur (inc i)))))
                      (let [qt (:qt qr)
                            skip (loop [i 0 sk true]
                                     (if (>= i n) sk
                                       (let [sm (loop [j 0 s2 0.0]
                                                      (if (>= j n) s2
                                                        (recur (inc j) (+ s2 (* (dget qt j i)
                                                                                 (aget ^doubles t j))))))
                                             wv (- (- (aget ^doubles fvec i) (aget ^doubles fvcold i)) sm)]
                                         (if (>= (absv wv) (* eps (+ (absv (aget ^doubles fvec i))
                                                                      (absv (aget ^doubles fvcold i)))))
                                           (do (aset ^doubles w i (double wv)) (recur (inc i) false))
                                           (do (aset ^doubles w i 0.0) (recur (inc i) sk))))))]
                        (if skip
                          [qr false]
                          (do (qr-qtmult qr w t)
                              (let [den (loop [i 0 s2 0.0]
                                              (if (>= i n) s2
                                                (let [v (aget ^doubles s i)]
                                                  (recur (inc i) (+ s2 (* v v))))))]
                                (loop [i 0]
                                  (when (< i n)
                                    (aset ^doubles s i (/ (aget ^doubles s i) den))
                                    (recur (inc i))))
                                [qr (qr-update qr t s)]))))))]
              (if failed
                [check 1]
                (do (qr-qtmult qr2 fvec p)
                    (loop [i 0]
                      (when (< i n)
                        (aset ^doubles p i (- (aget ^doubles p i)))
                        (recur (inc i))))
                    (let [r2 (:r qr2)]
                      (loop [i (dec n)]
                        (when (>= i 0)
                          (aset ^doubles g i (double (loop [j 0 s2 0.0]
                                                         (if (> j i) s2
                                                           (recur (inc j) (- s2 (* (dget r2 j i)
                                                                                    (aget ^doubles p j))))))))
                          (recur (dec i)))))
                    (dcopy! xold x n)
                    (dcopy! fvcold fvec n)
                    (let [fold f]
                      (qr-rsolve qr2 p p)
                      (let [[nf ncheck bad] (lnsrch sysid xold fold g p x stpmax)]
                        (if (not= bad 0)
                          [ncheck 1]
                          (do (dcopy! fvec (vecfunc sysid x) n)
                              (let [test (maxabs fvec n)]
                                (cond (< test tolf) [false flag]
                                      ncheck
                                      (if restrt
                                        [ncheck flag]
                                        (let [den (dmax nf (* 0.5 (double n)))
                                              t2 (loop [i 0 tt 0.0]
                                                     (if (>= i n) tt
                                                       (let [v (/ (* (absv (aget ^doubles g i))
                                                                       (dmax (absv (aget ^doubles x i)) 1.0))
                                                                    den)]
                                                         (recur (inc i) (if (> v tt) v tt)))))]
                                          (if (< t2 tolmin)
                                            [ncheck flag]
                                            (recur (inc its) nf ncheck true qr2
                                                   (if (= its maxits) 1 flag)))))
                                      :else
                                      (let [t3 (loop [i 0 tt 0.0]
                                                     (if (>= i n) tt
                                                       (let [v (/ (absv (- (aget ^doubles x i)
                                                                            (aget ^doubles xold i)))
                                                                    (dmax (absv (aget ^doubles x i)) 1.0))]
                                                         (recur (inc i) (if (> v tt) v tt)))))]
                                        (if (< t3 tolx)
                                          [ncheck flag]
                                          (recur (inc its) nf ncheck false qr2
                                                 (if (= its maxits) 1 flag)))))))))))))))))))

(defn print-x [^doubles x n zero]
  (str/join " " (map (fn [i] (fmt6 (canon (if zero 0.0 (aget ^doubles x i))))) (range n))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          sysid (nth toks 1)
          n (if (= sysid "SYS3") 3 2)
          x (double-array (map #(Double/parseDouble %) (subvec toks 2 (+ 2 n))))
          p0 (+ 2 n)
          res (cond (= op "MNEWT")
                    (let [ntrial (Long/parseLong (nth toks p0))
                          tolx (Double/parseDouble (nth toks (+ p0 1)))
                          tolf (Double/parseDouble (nth toks (+ p0 2)))
                          sing (mnewt sysid x ntrial tolx tolf)]
                      (str (print-x x n sing) " " (if sing 1 0)))
                    (or (= op "NEWT") (= op "BROYDN"))
                    (let [[check flag] (if (= op "NEWT") (newt sysid x) (broydn sysid x))]
                      (str (print-x x n (not= flag 0)) " " (if check 1 0) " " flag))
                    (= op "LNSRCH")
                    (let [pp (double-array (map #(Double/parseDouble %) (subvec toks p0 (+ p0 n))))
                          stpmax (Double/parseDouble (nth toks (+ p0 n)))
                          fvec (double-array n)
                          fold (fminfn sysid x fvec)
                          fj (fdjac sysid x fvec)
                          g (double-array n)
                          xn (double-array n)]
                      (loop [i 0]
                        (when (< i n)
                          (aset ^doubles g i (double (loop [j 0 s 0.0]
                                                         (if (>= j n) s
                                                           (recur (inc j) (+ s (* (dget fj j i)
                                                                                    (aget ^doubles fvec j))))))))
                          (recur (inc i))))
                      (let [[f check bad] (lnsrch sysid x fold g pp xn stpmax)]
                        (if (not= bad 0)
                          (str (print-x x n false) " " (fmt6 (canon fold)) " 1")
                          (str (print-x xn n false) " " (fmt6 (canon f)) " " (if check 1 0))))))]
      (when res (println res)))))
