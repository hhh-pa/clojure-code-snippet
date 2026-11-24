;; Multidimensional minimization by the Broyden-Fletcher-
;; Goldfarb-Shanno quasi-Newton method (NR dfpmin, BFGS
;; updating, lnsrch backtracking line searches).
;; Reference: Press et al., Numerical Recipes 3e, Section 10.9.
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

(defn dget [a i j] (aget ^doubles (nth a i) j))

(defn dset! [a i j v] (aset ^doubles (nth a i) j (double v)))

(defn dcopy! [^doubles dst ^doubles src n]
  (dotimes [i n] (aset ^doubles dst i (aget ^doubles src i))))

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

(defn mkdf [fname fp]
  (if (= fname "QUAD")
    (fn [^doubles x ^doubles df]
      (let [n (alength ^doubles x)]
        (dotimes [i n]
          (aset ^doubles df i (* 2.0 (- (aget ^doubles x i) (nth fp i)))))))
    (fn [^doubles x ^doubles df]
      (let [n (alength ^doubles x)]
        (aset ^doubles df 0 (- (* (* -400.0 (aget ^doubles x 0))
                                    (- (aget ^doubles x 1)
                                       (* (aget ^doubles x 0) (aget ^doubles x 0))))
                                 (* 2.0 (- 1.0 (aget ^doubles x 0)))))
        (loop [i 1]
          (when (< i (dec n))
            (aset ^doubles df i (- (- (* 200.0 (- (aget ^doubles x i)
                                                  (* (aget ^doubles x (dec i))
                                                     (aget ^doubles x (dec i)))))
                                        (* (* 400.0 (aget ^doubles x i))
                                           (- (aget ^doubles x (inc i))
                                              (* (aget ^doubles x i)
                                                 (aget ^doubles x i)))))
                                     (* 2.0 (- 1.0 (aget ^doubles x i)))))
            (recur (inc i))))
        (aset ^doubles df (dec n) (* 200.0 (- (aget ^doubles x (dec n))
                                              (* (aget ^doubles x (- n 2))
                                                 (aget ^doubles x (- n 2))))))))))

(defn lnsrch [f ^doubles xold fold ^doubles g ^doubles p ^doubles x stpmax f-in]
  (let [n (alength ^doubles xold)
        alf 1.0e-4
        tolx 2.220446049250313e-16
        sum (Math/sqrt (loop [i 0 s 0.0]
                             (if (>= i n) s
                               (let [v (aget ^doubles p i)] (recur (inc i) (+ s (* v v)))))))]
    (when (> sum stpmax)
      (let [r (/ stpmax sum)]
        (dotimes [i n] (aset ^doubles p i (* (aget ^doubles p i) r)))))
    (let [slope (loop [i 0 s 0.0]
                    (if (>= i n) s
                      (recur (inc i) (+ s (* (aget ^doubles g i) (aget ^doubles p i))))))]
      (if (>= slope 0.0)
        [f-in false 1]
        (let [test (loop [i 0 t 0.0]
                       (if (>= i n) t
                         (let [v (/ (absv (aget ^doubles p i))
                                    (dmax (absv (aget ^doubles xold i)) 1.0))]
                           (recur (inc i) (if (> v t) v t)))))
              alamin (/ tolx test)]
          (loop [alam 1.0 alam2 0.0 f2 0.0]
            (dotimes [i n]
              (aset ^doubles x i (+ (aget ^doubles xold i) (* alam (aget ^doubles p i)))))
            (let [fv (f x)]
              (cond (< alam alamin)
                    (do (dcopy! x xold n) [fv true 0])
                    (<= fv (+ fold (* (* alf alam) slope)))
                    [fv false 0]
                    :else
                    (let [tmplam (if (= alam 1.0)
                                   (/ (- slope) (* 2.0 (- (- fv fold) slope)))
                                   (let [raw (let [rhs1 (- (- fv fold) (* alam slope))
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
                      (recur (dmax tmplam (* 0.1 alam)) alam fv))))))))))

(defn dfpmin [f df point gtol]
  (let [n (count point)
        itmax 200
        eps 2.220446049250313e-16
        tolx (* 4 eps)
        stpmx 100.0
        p (double-array point)
        dg (double-array n)
        g (double-array n)
        hdg (double-array n)
        pnew (double-array n)
        xi (double-array n)
        hessin (vec (map (fn [_] (double-array n)) (range n)))]
    (df p g)
    (let [ss (loop [i 0 s 0.0]
                 (if (>= i n) s
                   (do (dotimes [j n] (dset! hessin i j 0.0))
                       (dset! hessin i i 1.0)
                       (aset ^doubles xi i (- (aget ^doubles g i)))
                       (let [v (aget ^doubles p i)]
                         (recur (inc i) (+ s (* v v)))))))
          stpmax (* stpmx (dmax (Math/sqrt ss) (double n)))]
      (loop [it 0 fret (f p)]
        (if (>= it itmax)
          [p fret (dec itmax) 1]
          (do (dcopy! pnew p n)
              (let [lr (lnsrch f p fret g xi pnew stpmax fret)
                    nf (nth lr 0)]
                (dotimes [i n]
                  (aset ^doubles xi i (- (aget ^doubles pnew i) (aget ^doubles p i)))
                  (aset ^doubles p i (aget ^doubles pnew i)))
                (let [t1 (loop [i 0 t 0.0]
                               (if (>= i n) t
                                 (let [v (/ (absv (aget ^doubles xi i))
                                            (dmax (absv (aget ^doubles p i)) 1.0))]
                                   (recur (inc i) (if (> v t) v t)))))]
                  (if (< t1 tolx)
                    [p nf it 0]
                    (do (dcopy! dg g n)
                        (df p g)
                        (let [den (dmax nf 1.0)
                              t2 (loop [i 0 t 0.0]
                                       (if (>= i n) t
                                         (let [v (/ (* (absv (aget ^doubles g i))
                                                         (dmax (absv (aget ^doubles p i)) 1.0))
                                                      den)]
                                           (recur (inc i) (if (> v t) v t)))))]
                          (if (< t2 gtol)
                            [p nf it 0]
                            (do (dotimes [i n]
                                  (aset ^doubles dg i (- (aget ^doubles g i)
                                                         (aget ^doubles dg i))))
                                (dotimes [i n]
                                  (aset ^doubles hdg i
                                        (loop [j 0 s 0.0]
                                              (if (>= j n) s
                                                (recur (inc j) (+ s (* (dget hessin i j)
                                                                       (aget ^doubles dg j))))))))
                                (let [[fac fae sumdg sumxi]
                                      (loop [i 0 a 0.0 e 0.0 sd 0.0 sx 0.0]
                                        (if (>= i n) [a e sd sx]
                                          (let [dgi (aget ^doubles dg i)
                                                xii (aget ^doubles xi i)]
                                            (recur (inc i)
                                                   (+ a (* dgi xii))
                                                   (+ e (* dgi (aget ^doubles hdg i)))
                                                   (+ sd (* dgi dgi))
                                                   (+ sx (* xii xii))))))]
                                  (when (> fac (Math/sqrt (* (* eps sumdg) sumxi)))
                                    (let [rf (/ 1.0 fac)
                                          rd (/ 1.0 fae)]
                                      (dotimes [i n]
                                        (aset ^doubles dg i (- (* rf (aget ^doubles xi i))
                                                                 (* rd (aget ^doubles hdg i)))))
                                      (dotimes [i n]
                                        (loop [j i]
                                          (when (< j n)
                                            (dset! hessin i j
                                                   (+ (dget hessin i j)
                                                      (+ (- (* (* rf (aget ^doubles xi i))
                                                                (aget ^doubles xi j))
                                                             (* (* rd (aget ^doubles hdg i))
                                                                (aget ^doubles hdg j)))
                                                         (* (* fae (aget ^doubles dg i))
                                                            (aget ^doubles dg j)))))
                                            (dset! hessin j i (dget hessin i j))
                                            (recur (inc j)))))))
                                  (dotimes [i n]
                                    (aset ^doubles xi i
                                          (- (loop [j 0 s 0.0]
                                                   (if (>= j n) s
                                                     (recur (inc j) (+ s (* (dget hessin i j)
                                                                              (aget ^doubles g j)))))))))
                                  (recur (inc it) nf)))))))))))))))

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
          gtol (Double/parseDouble (nth toks (+ pp n)))
          res (when (= op "BFGS")
                (let [f (mkfunc fname fp)
                      df (mkdf fname fp)
                      r (dfpmin f df point gtol)
                      pmin (nth r 0)
                      fret (nth r 1)
                      it (nth r 2)
                      flag (nth r 3)]
                  (if (not= flag 0)
                    (str (str/join " " (repeat n "0.000000")) " 0.000000 " it " 1")
                    (str (str/join " " (map #(fmt6 (canon (aget ^doubles pmin %))) (range n)))
                         " " (fmt6 (canon fret)) " " it " 0"))))]
      (when res (println res)))))
