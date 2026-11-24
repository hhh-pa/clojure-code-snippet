;; Linear prediction: Burg LP coefficients (memcof), stabilization by
;; root reflection (fixrts), and extrapolation (predic).
;; Reference: Press et al., Numerical Recipes 3e, Section 13.7
;; (memcof/fixrts/predic, vendoring zroots+laguer from 9.5 with the
;; explicit real/imag complex flops of math/roots/poly_roots).
;; Documented decisions: n >= 2 and 1 <= m < n are enforced (the
;; book indexes out of bounds for n = 1 and divides 0/0 for
;; m >= n); an exact-zero denom flags (the book yields NaN);
;; PREDIC needs ndata >= m >= 1 (the book overruns or
;; dereferences an empty register otherwise).
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

(defn cxadd [[ar ai] [br bi]] [(+ ar br) (+ ai bi)])
(defn cxsub [[ar ai] [br bi]] [(- ar br) (- ai bi)])
(defn cxmul [[ar ai] [br bi]] [(- (* ar br) (* ai bi)) (+ (* ar bi) (* ai br))])
(defn cxdiv [[ar ai] [br bi]]
  (let [d (+ (* br br) (* bi bi))]
    [(/ (+ (* ar br) (* ai bi)) d) (/ (- (* ai br) (* ar bi)) d)]))
(defn cxscale [[ar ai] s] [(* ar s) (* ai s)])
(defn cxabs [[ar ai]] (Math/sqrt (+ (* ar ar) (* ai ai))))
(defn cxsqrt [[wr wi]]
  (if (= wi 0.0)
    (if (>= wr 0.0) [(Math/sqrt wr) 0.0] [0.0 (Math/sqrt (- wr))])
    (let [r (cxabs [wr wi])
          re (Math/sqrt (* (+ r wr) 0.5))
          im (Math/sqrt (* (- r wr) 0.5))]
      [re (if (>= wi 0.0) im (- im))])))
(defn cxpolar [r th] [(* r (Math/cos th)) (* r (Math/sin th))])

(def maceps 2.220446049250313e-16)
(def fracv [0.0 0.5 0.25 0.75 0.13 0.38 0.62 0.88 1.0])

(defn laguer [a x0]
  (let [m (dec (count a))]
    (loop [iter 1 x x0 its 0]
      (if (> iter 80)
        [x its 1]
        (let [b (nth a m)
              err (cxabs b)
              abx (cxabs x)
              [b d f err] (loop [j (dec m) b b d [0.0 0.0] f [0.0 0.0] err err]
                            (if (< j 0)
                              [b d f err]
                              (let [f (cxadd (cxmul x f) d)
                                    d (cxadd (cxmul x d) b)
                                    b (cxadd (cxmul x b) (nth a j))]
                                (recur (dec j) b d f (+ (cxabs b) (* abx err))))))
              err (* err maceps)]
          (if (<= (cxabs b) err)
            [x iter 0]
            (let [g (cxdiv d b)
                  g2 (cxmul g g)
                  h (cxsub g2 (cxscale (cxdiv f b) 2.0))
                  sq (cxsqrt (cxscale (cxsub (cxscale h (double m)) g2) (double (dec m))))
                  gp (cxadd g sq)
                  gm (cxsub g sq)
                  abp (cxabs gp)
                  abm (cxabs gm)
                  gp (if (< abp abm) gm gp)
                  mx (if (< abp abm) abm abp)
                  dx (if (> mx 0.0)
                       (cxdiv [(double m) 0.0] gp)
                       (cxpolar (+ 1.0 abx) (double iter)))
                  x1 (cxsub x dx)]
              (if (and (= (first x) (first x1)) (= (second x) (second x1)))
                [x iter 0]
                (recur (inc iter)
                       (if (not= (mod iter 10) 0)
                         x1
                         (cxsub x (cxscale dx (nth fracv (quot iter 10)))))
                       iter)))))))))

(defn zroots [a]
  (let [m (dec (count a))
        eps 1.0e-14]
    (loop [j (dec m) ad (vec a) roots (vec (repeat m nil)) flag 0]
      (if (or (< j 0) (not= flag 0))
        (if (not= flag 0)
          [roots 1]
          (let [[roots flag]
                (loop [j 0 roots roots flag 0]
                  (if (or (>= j m) (not= flag 0))
                    [roots flag]
                    (let [[x _ fl] (laguer a (nth roots j))]
                      (recur (inc j) (assoc roots j x) fl))))]
            (if (not= flag 0)
              [roots 1]
              [(loop [j 1 roots roots]
                 (if (>= j m)
                   roots
                   (let [x (nth roots j)
                         roots (loop [i (dec j) roots roots]
                                 (if (< i 0)
                                   (assoc roots (inc i) x)
                                   (if (<= (first (nth roots i)) (first x))
                                     (assoc roots (inc i) x)
                                     (recur (dec i) (assoc roots (inc i) (nth roots i))))))]
                     (recur (inc j) roots))))
               0])))
        (let [[x _ fl] (laguer (subvec ad 0 (+ j 2)) [0.0 0.0])]
          (if (not= fl 0)
            [roots 1]
            (let [x (if (<= (absv (second x)) (* (* 2.0 eps) (absv (first x))))
                      [(first x) 0.0]
                      x)
                  roots (assoc roots j x)
                  b (nth ad (inc j))
                  ad (loop [jj j b b ad ad]
                       (if (< jj 0)
                         ad
                         (let [c (nth ad jj)]
                           (recur (dec jj) (cxadd (cxmul x b) c) (assoc ad jj b)))))]
              (recur (dec j) ad roots 0))))))))

(defn memcof [data m]
  (let [n (count data)]
    (if (or (< n 2) (< m 1) (>= m n))
      nil
      (let [d (double-array m)
            wk1 (double-array n)
            wk2 (double-array n)
            wkm (double-array m)
            p (loop [j 0 s 0.0]
                (if (>= j n) s
                  (recur (inc j) (+ s (* (nth data j) (nth data j))))))]
        (loop [k 0 xms (/ p (double n))]
          (if (>= k m)
            nil
            (let [_ (do
                      (when (= k 0)
                        (aset ^doubles wk1 0 (nth data 0))
                        (aset ^doubles wk2 (- n 2) (nth data (dec n)))
                        (dotimes [jj (- n 2)]
                          (let [j (inc jj)]
                            (aset ^doubles wk1 j (nth data j))
                            (aset ^doubles wk2 (dec j) (nth data j))))))
                  [num denom] (loop [j 0 num 0.0 denom 0.0]
                                (if (>= j (- n k 1))
                                  [num denom]
                                  (recur (inc j)
                                         (+ num (* (aget ^doubles wk1 j)
                                                     (aget ^doubles wk2 j)))
                                         (+ denom (+ (* (aget ^doubles wk1 j)
                                                          (aget ^doubles wk1 j))
                                                       (* (aget ^doubles wk2 j)
                                                          (aget ^doubles wk2 j)))))))]
              (if (= denom 0.0)
                nil
                (do
                  (aset ^doubles d k (/ (* 2.0 num) denom))
                  (let [t (aget ^doubles d k)
                        xms (* xms (- 1.0 (* t t)))]
                    (dotimes [i k]
                      (aset ^doubles d i
                            (- (aget ^doubles wkm i)
                               (* (aget ^doubles d k)
                                  (aget ^doubles wkm (- (- k 1) i))))))
                    (if (= k (dec m))
                      [(vec d) xms]
                      (do
                        (dotimes [i (inc k)]
                          (aset ^doubles wkm i (aget ^doubles d i)))
                        (dotimes [jj (- n k 2)]
                          (aset ^doubles wk1 jj
                                (- (aget ^doubles wk1 jj)
                                   (* (aget ^doubles wkm k)
                                      (aget ^doubles wk2 jj))))
                          (aset ^doubles wk2 jj
                                (- (aget ^doubles wk2 (inc jj))
                                   (* (aget ^doubles wkm k)
                                      (aget ^doubles wk1 (inc jj))))))
                        (recur (inc k) xms)))))))))))))

(defn fixrts [dv]
  (let [m (count dv)]
    (if (< m 1)
      nil
      (let [a (vec (concat (map (fn [j] [(- (nth dv (- (- m 1) j))) 0.0])
                                 (range m))
                            [[1.0 0.0]]))
            [roots flag] (zroots a)]
        (if (not= flag 0)
          nil
          (let [roots (vec (map (fn [r]
                                  (if (> (cxabs r) 1.0)
                                    (cxdiv [1.0 0.0] [(first r) (- (second r))])
                                    r))
                                roots))
                a (loop [j 1
                         a (assoc (vec a) 0 [(- (first (nth roots 0)))
                                                (- (second (nth roots 0)))]
                                  1 [1.0 0.0])]
                    (if (>= j m)
                      a
                      (let [a (assoc a (+ j 1) [1.0 0.0])
                            a (loop [i j a a]
                                (if (< i 1)
                                  a
                                  (recur (dec i)
                                         (assoc a i
                                                (cxsub (nth a (dec i))
                                                       (cxmul (nth roots j)
                                                              (nth a i)))))))]
                        (recur (inc j)
                               (assoc a 0
                                      (cxmul [(- (first (nth roots j)))
                                              (- (second (nth roots j)))]
                                             (nth a 0)))))))]
            (vec (map (fn [j] (- (first (nth a (- (- m 1) j)))))
                        (range m)))))))))

(defn predic [data dv nfut]
  (let [ndata (count data)
        m (count dv)]
    (if (or (< ndata m) (< m 1))
      nil
      (loop [j 0
             reg (vec (map (fn [jj] (nth data (- (- ndata 1) jj))) (range m)))
             out []]
        (if (>= j nfut)
          out
          (let [s (loop [k 0 sum 0.0]
                    (if (>= k m) sum
                      (recur (inc k) (+ sum (* (nth dv k) (nth reg k))))))
                reg (vec (concat [s] (subvec reg 0 (dec m))))]
            (recur (inc j) reg (conj out s))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (cond
        (= op "MEMCOF")
        (let [n (Long/parseLong (nth toks 1))
              m (Long/parseLong (nth toks 2))
              data (vec (map #(Double/parseDouble %) (subvec toks 3 (+ 3 (max 0 n)))))
              r (memcof data m)]
          (if r
            (let [[d xms] r]
              (println (str (str/join " " (concat (map #(fmt6 (canon %)) d)
                                                  [(fmt6 (canon xms))]))
                            " 0")))
            (let [w (max 0 (+ m 1))]
              (println (str (str/join " " (concat (repeat w "0.000000") ["1"])))))))
        (= op "FIXRTS")
        (let [m (Long/parseLong (nth toks 1))
              d (vec (map #(Double/parseDouble %) (subvec toks 2 (+ 2 (max 0 m)))))
              r (fixrts d)]
          (if r
            (println (str (str/join " " (map #(fmt6 (canon %)) r)) " 0"))
            (let [w (max 0 m)]
              (println (str (str/join " " (concat (repeat w "0.000000") ["1"])))))))
        (= op "PREDIC")
        (let [ndata (Long/parseLong (nth toks 1))
              m (Long/parseLong (nth toks 2))
              nfut (Long/parseLong (nth toks 3))
              data (vec (map #(Double/parseDouble %) (subvec toks 4 (+ 4 (max 0 ndata)))))
              d (vec (map #(Double/parseDouble %)
                           (subvec toks (+ 4 (max 0 ndata))
                                   (+ 4 (max 0 ndata) (max 0 m)))))
              r (predic data d nfut)]
          (if r
            (println (str (str/join " " (map #(fmt6 (canon %)) r)) " 0"))
            (let [w (max 0 nfut)]
              (println (str (str/join " " (concat (repeat w "0.000000") ["1"])))))))))))
