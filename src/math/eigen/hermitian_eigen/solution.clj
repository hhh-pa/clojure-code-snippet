;; All eigenvalues and eigenvectors of a Hermitian matrix
;; via reduction to a real symmetric 2n x 2n problem
;; (solved by Householder+QL), choosing one from each
;; doubled eigenpair.
;; Reference: Press et al., Numerical Recipes 3e, Section 11.5.
;; No online-judge problem found -- fixtures are self-generated
;; (numpy oracle), no header link.
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

(defn sgn [a b] (if (>= b 0.0) (absv a) (- (absv a))))

(defn dget [a i j] (aget ^doubles (nth a i) j))

(defn dset! [a i j v] (aset ^doubles (nth a i) j (double v)))

(defn dmat [n m] (vec (repeatedly n #(double-array m))))

(defn mcopy [a]
  (let [n (count a)]
    (vec (map (fn [row]
                (let [r (double-array n)]
                  (dotimes [i n] (aset ^doubles r i (aget ^doubles row i)))
                  r))
              a))))

(defn eigsrt [^doubles d v]
  (let [n (alength ^doubles d)]
    (loop [i 0]
      (when (< i (dec n))
        (let [[k p] (loop [j i k i p (aget ^doubles d i)]
                        (if (>= j n) [k p]
                          (if (>= (aget ^doubles d j) p)
                            (recur (inc j) j (aget ^doubles d j))
                            (recur (inc j) k p))))]
          (when (not= k i)
            (aset ^doubles d k (aget ^doubles d i))
            (aset ^doubles d i p)
            (dotimes [j n]
              (let [t (dget v j i)]
                (dset! v j i (dget v j k))
                (dset! v j k t)))))
        (recur (inc i))))))

(defn pythag [a b]
  (let [absa (absv a) absb (absv b)]
    (if (> absa absb)
      (* absa (Math/sqrt (+ 1.0 (* (/ absb absa) (/ absb absa)))))
      (if (= absb 0.0)
        0.0
        (* absb (Math/sqrt (+ 1.0 (* (/ absa absb) (/ absa absb)))))))))

(defn tred2 [z ^doubles d ^doubles e]
  (let [n (count z)]
    (loop [i (dec n)]
      (when (> i 0)
        (let [l (dec i)
              h (if (> l 0)
                  (let [scale (loop [k 0 s 0.0]
                                    (if (>= k i) s
                                      (recur (inc k) (+ s (absv (dget z i k))))))]
                    (if (= scale 0.0)
                      (do (aset ^doubles e i (dget z i l)) 0.0)
                      (let [h (loop [k 0 h 0.0]
                                    (if (>= k i) h
                                      (do (dset! z i k (/ (dget z i k) scale))
                                          (let [v (dget z i k)]
                                            (recur (inc k) (+ h (* v v)))))))
                            f (dget z i l)
                            g (if (>= f 0.0) (- (Math/sqrt h)) (Math/sqrt h))]
                        (aset ^doubles e i (* scale g))
                        (let [h (- h (* f g))]
                          (dset! z i l (- f g))
                          (let [f (loop [j 0 f 0.0]
                                        (if (>= j i) f
                                          (do (dset! z j i (/ (dget z i j) h))
                                              (let [g (loop [k 0 s 0.0]
                                                              (if (>= k i) s
                                                                (recur (inc k)
                                                                       (+ s (* (if (<= k j)
                                                                                 (dget z j k)
                                                                                 (dget z k j))
                                                                              (dget z i k))))))]
                                                (aset ^doubles e j (/ g h))
                                                (recur (inc j) (+ f (* (aget ^doubles e j)
                                                                         (dget z i j))))))))]
                            (let [hh (/ f (+ h h))]
                              (loop [j 0]
                                (when (< j i)
                                  (let [f (dget z i j)
                                        g (- (aget ^doubles e j) (* hh f))]
                                    (aset ^doubles e j g)
                                    (loop [k 0]
                                      (when (<= k j)
                                        (dset! z j k (- (dget z j k)
                                                        (+ (* f (aget ^doubles e k))
                                                           (* g (dget z i k)))))
                                        (recur (inc k))))
                                    (recur (inc j))))))
                            h)))))
                  (do (aset ^doubles e i (dget z i l)) 0.0))]
          (aset ^doubles d i h))
        (recur (dec i))))
    (aset ^doubles d 0 0.0)
    (aset ^doubles e 0 0.0)
    (loop [i 0]
      (when (< i n)
        (when (not= (aget ^doubles d i) 0.0)
          (loop [j 0]
            (when (< j i)
              (let [g (loop [k 0 s 0.0]
                            (if (>= k i) s
                              (recur (inc k) (+ s (* (dget z i k) (dget z k j))))))]
                (loop [k 0]
                  (when (< k i)
                    (dset! z k j (- (dget z k j) (* g (dget z k i))))
                    (recur (inc k)))))
              (recur (inc j)))))
        (aset ^doubles d i (dget z i i))
        (dset! z i i 1.0)
        (dotimes [j i]
          (dset! z j i 0.0)
          (dset! z i j 0.0))
        (recur (inc i))))))

(defn tqli [^doubles d ^doubles e z]
  (let [n (alength ^doubles d)
        eps 2.220446049250313e-16]
    (loop [i 1]
      (when (< i n)
        (aset ^doubles e (dec i) (aget ^doubles e i))
        (recur (inc i))))
    (aset ^doubles e (dec n) 0.0)
    (loop [l 0]
      (if (>= l n)
        true
        (let [r (loop [iter 0]
                  (let [m (loop [m l]
                                (if (>= m (dec n)) m
                                  (let [dd (+ (absv (aget ^doubles d m))
                                              (absv (aget ^doubles d (inc m))))]
                                    (if (<= (absv (aget ^doubles e m)) (* eps dd))
                                      m
                                      (recur (inc m))))))]
                    (if (= m l)
                      :done
                      (if (>= iter 30)
                        :fail
                        (let [g (/ (- (aget ^doubles d (inc l)) (aget ^doubles d l))
                                   (* 2.0 (aget ^doubles e l)))
                              r (pythag g 1.0)
                              g (+ (- (aget ^doubles d m) (aget ^doubles d l))
                                   (/ (aget ^doubles e l) (+ g (sgn r g))))
                              res (loop [i (dec m) s 1.0 c 1.0 p 0.0 g g r 0.0]
                                      (if (< i l)
                                        [s c p g r i]
                                        (let [f (* s (aget ^doubles e i))
                                              b (* c (aget ^doubles e i))
                                              r (pythag f g)]
                                          (aset ^doubles e (inc i) r)
                                          (if (= r 0.0)
                                            (do (aset ^doubles d (inc i)
                                                      (- (aget ^doubles d (inc i)) p))
                                                (aset ^doubles e m 0.0)
                                                [s c p g r i])
                                            (let [s (/ f r)
                                                  c (/ g r)
                                                  g (- (aget ^doubles d (inc i)) p)
                                                  r (+ (* (- (aget ^doubles d i) g) s)
                                                       (* (* 2.0 c) b))
                                                  p (* s r)]
                                              (aset ^doubles d (inc i) (+ g p))
                                              (let [g (- (* c r) b)]
                                                (dotimes [k n]
                                                  (let [f (dget z k (inc i))]
                                                    (dset! z k (inc i)
                                                           (+ (* s (dget z k i)) (* c f)))
                                                    (dset! z k i
                                                           (- (* c (dget z k i)) (* s f)))))
                                                (recur (dec i) s c p g r)))))))]
                          (if (and (= (nth res 4) 0.0) (>= (nth res 5) l))
                            (recur (inc iter))
                            (do (aset ^doubles d l (- (aget ^doubles d l) (nth res 2)))
                                (aset ^doubles e l (nth res 3))
                                (aset ^doubles e m 0.0)
                                (recur (inc iter)))))))))]
          (if (= r :fail)
            false
            (recur (inc l))))))))

(defn symmeig-matrix [a]
  (let [n (count a)
        z (mcopy a)
        d (double-array n)
        e (double-array n)]
    (tred2 z d e)
    (when (tqli d e z)
      (eigsrt d z)
      [d z])))

(defn symmeig-tridiag [^doubles dd ^doubles ee]
  (let [n (alength ^doubles dd)
        z (dmat n n)
        d (double-array n)
        e (double-array n)]
    (dotimes [i n]
      (aset ^doubles d i (aget ^doubles dd i))
      (aset ^doubles e i (aget ^doubles ee i))
      (dset! z i i 1.0))
    (when (tqli d e z)
      (eigsrt d z)
      [d z])))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          n (Long/parseLong (nth toks 1))
          vals (vec (map #(Double/parseDouble %) (subvec toks 2)))
          amat (vec (map (fn [i] (double-array (subvec vals (* i n) (* (inc i) n))))
                          (range n)))
          bmat (vec (map (fn [i] (double-array (subvec vals (+ (* n n) (* i n))
                                                          (+ (* n n) (* (inc i) n)))))
                          (range n)))
          res (when (= op "HERMITIAN")
                (let [m (* 2 n)
                      aug (dmat m m)]
                  (dotimes [i n]
                    (dotimes [j n]
                      (dset! aug i j (aget ^doubles (nth amat i) j))
                      (dset! aug i (+ j n) (- (aget ^doubles (nth bmat i) j)))
                      (dset! aug (+ i n) j (aget ^doubles (nth bmat i) j))
                      (dset! aug (+ i n) (+ j n) (aget ^doubles (nth amat i) j))))
                  [:known (symmeig-matrix aug)]))]
      (when res
        (let [r (nth res 1)]
          (if (nil? r)
            (println (str (str/join " " (repeat (+ n (* 2 (* n n))) "0.000000")) " 1"))
            (let [d (nth r 0)
                  z (nth r 1)]
              (println (str (str/join " "
                                       (concat (map #(fmt6 (canon (aget ^doubles d (* 2 %))))
                                                     (range n))
                                               (mapcat (fn [k] (map #(fmt6 (canon (dget z % (* 2 k))))
                                                                     (range n)))
                                                        (range n))
                                               (mapcat (fn [k] (map #(fmt6 (canon (dget z (+ % n)
                                                                                      (* 2 k))))
                                                                     (range n)))
                                                        (range n))))
                             " 0")))))))))
