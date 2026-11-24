;; Linear programming by the book's basic primal-dual
;; infeasible interior-point method (routine intpt; dense
;; LU substitutes the book's external sparse LDL+AMD
;; backend, a Webnote-only performance optimization).
;; Reference: Press et al., Numerical Recipes 3e, Section 10.11.
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

(defn dmin [a b] (if (< a b) a b))

(defn dget [a i j] (aget ^doubles (nth a i) j))

(defn dset! [a i j v] (aset ^doubles (nth a i) j (double v)))

(defn dmat [n m] (vec (repeatedly n #(double-array m))))

(defn mcopy [a]
  (vec (map (fn [row]
              (let [n (alength ^doubles row)
                    r (double-array n)]
                (dotimes [i n] (aset ^doubles r i (aget ^doubles row i)))
                r))
            a)))

(defn dotprod [^doubles x ^doubles y]
  (let [n (alength ^doubles x)]
    (loop [i 0 s 0.0]
      (if (>= i n) s
        (recur (inc i) (+ s (* (aget ^doubles x i) (aget ^doubles y i))))))))

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
            (let [imax (loop [i k imax k b 0.0]
                           (if (>= i n) imax
                             (let [t (* (aget ^doubles vv i) (absv (dget lu i k)))]
                               (if (> t b) (recur (inc i) i t) (recur (inc i) imax b)))))]
              (when (not= k imax)
                (dotimes [j n]
                  (let [t (dget lu imax j)]
                    (dset! lu imax j (dget lu k j))
                    (dset! lu k j t)))
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

(defn lu-solve [lum ^doubles b ^doubles x]
  (let [n (:n lum) lu (:lu lum) indx (:indx lum)]
    (dotimes [i n] (aset ^doubles x i (aget ^doubles b i)))
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

(defn intpt [a ^doubles b ^doubles c ^doubles x]
  (let [m (count a)
        n (alength ^doubles x)
        maxits 200
        eps 1.0e-6
        sigma 0.9
        delta 0.02
        big 1.7976931348623157e308
        y (double-array m)
        z (double-array n)
        ax (double-array m)
        aty (double-array n)
        rp (double-array m)
        rd (double-array n)
        d (double-array n)
        dx (double-array n)
        dy (double-array m)
        dz (double-array n)
        rhs (double-array m)
        tempm (double-array m)
        tempn (double-array n)
        at (dmat n m)
        mat (dmat m m)]
    (dotimes [i m]
      (dotimes [j n]
        (dset! at j i (dget a i j))))
    (let [rpfact (+ 1.0 (Math/sqrt (dotprod b b)))
          rdfact (+ 1.0 (Math/sqrt (dotprod c c)))]
      (dotimes [j n]
        (aset ^doubles x j 1000.0)
        (aset ^doubles z j 1000.0))
      (dotimes [i m] (aset ^doubles y i 1000.0))
      (loop [iter 0 normrp-old big normrd-old big]
        (if (>= iter maxits)
          3
          (do (dotimes [i m]
                (aset ^doubles ax i (loop [j 0 s 0.0]
                                         (if (>= j n) s
                                           (recur (inc j) (+ s (* (dget a i j)
                                                                   (aget ^doubles x j))))))))
              (dotimes [i m] (aset ^doubles rp i (- (aget ^doubles ax i) (aget ^doubles b i))))
              (let [normrp (/ (Math/sqrt (dotprod rp rp)) rpfact)]
                (dotimes [j n]
                  (aset ^doubles aty j (loop [i 0 s 0.0]
                                            (if (>= i m) s
                                              (recur (inc i) (+ s (* (dget at j i)
                                                                      (aget ^doubles y i))))))))
                (dotimes [j n]
                  (aset ^doubles rd j (+ (+ (aget ^doubles aty j) (aget ^doubles z j))
                                            (- (aget ^doubles c j)))))
                (let [normrd (/ (Math/sqrt (dotprod rd rd)) rdfact)
                      gamma (dotprod x z)
                      mu (/ (* delta gamma) (double n))
                      primal-obj (dotprod c x)
                      gamma-norm (/ gamma (+ 1.0 (absv primal-obj)))]
                  (cond (and (< normrp eps) (< normrd eps) (< gamma-norm eps)) 0
                        (and (> normrp (* 1000.0 normrp-old)) (> normrp eps)) 1
                        (and (> normrd (* 1000.0 normrd-old)) (> normrd eps)) 2
                        :else
                        (do (dotimes [j n]
                              (aset ^doubles d j (/ (aget ^doubles x j) (aget ^doubles z j))))
                            (dotimes [i m]
                              (dotimes [k m]
                                (dset! mat i k
                                       (loop [j 0 s 0.0]
                                             (if (>= j n) s
                                               (recur (inc j) (+ s (* (* (dget a i j)
                                                                          (aget ^doubles d j))
                                                                       (dget a k j)))))))))
                            (dotimes [j n]
                              (aset ^doubles tempn j
                                    (- (- (aget ^doubles x j)
                                          (/ mu (aget ^doubles z j)))
                                       (* (aget ^doubles d j) (aget ^doubles rd j)))))
                            (dotimes [i m]
                              (aset ^doubles tempm i
                                    (loop [j 0 s 0.0]
                                          (if (>= j n) s
                                            (recur (inc j) (+ s (* (dget a i j)
                                                                     (aget ^doubles tempn j))))))))
                            (dotimes [i m]
                              (aset ^doubles rhs i (+ (- (aget ^doubles rp i))
                                                        (aget ^doubles tempm i))))
                            (let [alu (new-lu mat)]
                              (if (nil? alu)
                                3
                                (do (lu-solve alu rhs dy)
                                    (dotimes [j n]
                                      (aset ^doubles tempn j
                                            (loop [i 0 s 0.0]
                                                  (if (>= i m) s
                                                    (recur (inc i) (+ s (* (dget at j i)
                                                                             (aget ^doubles dy i))))))))
                                    (dotimes [j n]
                                      (aset ^doubles dz j (- (- (aget ^doubles tempn j))
                                                                (aget ^doubles rd j))))
                                    (dotimes [j n]
                                      (aset ^doubles dx j
                                            (- (+ (- (* (aget ^doubles d j)
                                                         (aget ^doubles dz j)))
                                                  (/ mu (aget ^doubles z j)))
                                               (aget ^doubles x j))))
                                    (let [alpha-p (loop [j 0 a 1.0]
                                                      (if (>= j n) a
                                                        (recur (inc j)
                                                               (if (< (+ (aget ^doubles x j)
                                                                          (* a (aget ^doubles dx j)))
                                                                       0.0)
                                                                 (/ (- (aget ^doubles x j))
                                                                    (aget ^doubles dx j))
                                                                 a))))
                                          alpha-d (loop [j 0 a 1.0]
                                                      (if (>= j n) a
                                                        (recur (inc j)
                                                               (if (< (+ (aget ^doubles z j)
                                                                          (* a (aget ^doubles dz j)))
                                                                       0.0)
                                                                 (/ (- (aget ^doubles z j))
                                                                    (aget ^doubles dz j))
                                                                 a))))
                                          ap (dmin (* alpha-p sigma) 1.0)
                                          ad (dmin (* alpha-d sigma) 1.0)]
                                      (dotimes [j n]
                                        (aset ^doubles x j (+ (aget ^doubles x j)
                                                               (* ap (aget ^doubles dx j))))
                                        (aset ^doubles z j (+ (aget ^doubles z j)
                                                               (* ad (aget ^doubles dz j)))))
                                      (dotimes [i m]
                                        (aset ^doubles y i (+ (aget ^doubles y i)
                                                               (* ad (aget ^doubles dy i)))))
                                      (recur (inc iter) normrp normrd)))))))))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          m (Long/parseLong (nth toks 1))
          n (Long/parseLong (nth toks 2))
          vals (vec (map #(Double/parseDouble %) (subvec toks 3)))
          a (vec (map (fn [i] (double-array (subvec vals (* i n) (* (inc i) n))))
                       (range m)))
          b (double-array (subvec vals (* m n) (+ (* m n) m)))
          c (double-array (subvec vals (+ (* m n) m) (+ (* m n) m n)))
          res (when (= op "INTPT")
                (let [x (double-array n)
                      status (intpt a b c x)]
                  (str (str/join " " (map #(fmt6 (canon (aget ^doubles x %))) (range n)))
                       " " status)))]
      (when res (println res)))))
