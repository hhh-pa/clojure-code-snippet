;; Polynomial roots (NR 9.5).
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

(defn poldiv [u v]
  (let [n (dec (count u))
        nv (loop [nv (dec (count v))]
             (if (or (< nv 0) (not= (nth v nv) 0.0)) nv (recur (dec nv))))
        r (vec u)]
    (if (< nv 0)
      [(vec (repeat (count u) 0.0)) r]
      (loop [k (- n nv) q (vec (repeat (count u) 0.0)) r r]
        (if (< k 0)
          [q (vec (concat (subvec r 0 nv) (repeat (- (inc n) nv) 0.0)))]
          (let [qk (/ (nth r (+ nv k)) (nth v nv))
                r (loop [j (+ (+ nv k) -1) r r]
                    (if (< j k)
                      r
                      (recur (dec j) (assoc r j (- (nth r j) (* qk (nth v (- j k))))))))]
            (recur (dec k) (assoc q k qk) r)))))))

(defn zroots [a polish]
  (let [m (dec (count a))
        eps 1.0e-14]
    (loop [j (dec m) ad (vec a) roots (vec (repeat m nil)) flag 0]
      (if (or (< j 0) (not= flag 0))
        (if (not= flag 0)
          [roots 1]
          (let [[roots flag] (if (not= polish 0)
                               (loop [j 0 roots roots flag 0]
                                 (if (or (>= j m) (not= flag 0))
                                   [roots flag]
                                   (let [[x _ fl] (laguer a (nth roots j))]
                                     (recur (inc j) (assoc roots j x) fl))))
                               [roots 0])]
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

(defn qroot [p b0 c0 eps]
  (let [tiny 1.0e-14]
    (loop [iter 0 b b0 c c0]
      (if (>= iter 20)
        [b c 1]
        (let [d [c b 1.0]
              [_ rem] (poldiv p d)
              s (nth rem 0)
              r (nth rem 1)
              [_ rem2] (poldiv (first (poldiv p d)) d)
              rc (- (nth rem2 1))
              sb (* (- c) rc)
              sc (- (nth rem2 0))
              rb (+ (* (- b) rc) sc)
              dv (/ 1.0 (- (* sb rc) (* sc rb)))
              delb (* (- (* r sc) (* s rc)) dv)
              delc (* (+ (* (- r) sb) (* s rb)) dv)
              b (+ b delb)
              c (+ c delc)]
          (if (and (or (<= (absv delb) (* eps (absv b))) (< (absv b) tiny))
                   (or (<= (absv delc) (* eps (absv c))) (< (absv c) tiny)))
            [b c 0]
            (recur (inc iter) b c)))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (println
       (str/join " "
        (cond
          (= op "ZROOTS")
          (let [m (Long/parseLong (nth toks 1))
                a (vec (for [i (range (inc m))]
                         [(Double/parseDouble (nth toks (+ 2 (* 2 i))))
                          (Double/parseDouble (nth toks (+ 3 (* 2 i))))]))
                polish (Long/parseLong (nth toks (+ 2 (* 2 (inc m)))))
                [roots flag] (zroots a polish)]
            (concat (mapcat (fn [j]
                              (let [[r s] (if (not= flag 0) [0.0 0.0] (nth roots j))]
                                [(fmt6 (canon r)) (fmt6 (canon s))]))
                            (range m))
                    [(str flag)]))
          (= op "LAGUER")
          (let [m (Long/parseLong (nth toks 1))
                a (vec (for [i (range (inc m))]
                         [(Double/parseDouble (nth toks (+ 2 (* 2 i))))
                          (Double/parseDouble (nth toks (+ 3 (* 2 i))))]))
                base (+ 2 (* 2 (inc m)))
                x [(Double/parseDouble (nth toks base)) (Double/parseDouble (nth toks (inc base)))]
                [x its flag] (laguer a x)]
            (if (not= flag 0)
              ["0.000000" "0.000000" "0" "1"]
              [(fmt6 (canon (first x))) (fmt6 (canon (second x))) (str its) "0"]))
          (= op "QROOT")
          (let [n (Long/parseLong (nth toks 1))
                p (vec (map #(Double/parseDouble %) (subvec toks 2 (+ 3 n))))
                b (Double/parseDouble (nth toks (+ 3 n)))
                c (Double/parseDouble (nth toks (+ 4 n)))
                eps (Double/parseDouble (nth toks (+ 5 n)))
                [b c flag] (qroot p b c eps)]
            (if (not= flag 0)
              ["0.000000" "0.000000" "1"]
              [(fmt6 (canon b)) (fmt6 (canon c)) "0"]))))))))
