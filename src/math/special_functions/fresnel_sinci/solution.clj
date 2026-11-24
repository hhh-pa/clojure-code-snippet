;; Fresnel and cosine/sine integrals (NR 6.8).
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

(def eps 2.220446049250313e-16)
(def fzmin 2.2250738585072014e-308)
(def czmin (* 2.2250738585072014e-308 4.0))
(def big (* 1.7976931348623157e308 eps))

(defn cxadd [[ar ai] [br bi]] [(+ ar br) (+ ai bi)])
(defn cxsub [[ar ai] [br bi]] [(- ar br) (- ai bi)])
(defn cxmul [[ar ai] [br bi]] [(- (* ar br) (* ai bi)) (+ (* ar bi) (* ai br))])
(defn cxdiv [[ar ai] [br bi]]
  (let [d (+ (* br br) (* bi bi))]
    [(/ (+ (* ar br) (* ai bi)) d) (/ (- (* ai br) (* ar bi)) d)]))

(defn frenel [x]
  (let [piby2 (/ Math/PI 2.0)
        ax (absv x)]
    (cond (< ax (Math/sqrt fzmin)) [ax 0.0]
          (<= ax 1.5)
          (let [fact (* piby2 ax ax)]
            (loop [k 1 term ax sum 0.0 sums 0.0 sumc ax sign 1.0 odd true n 3]
              (if (> k 100) [sumc sums]
                  (let [term (* term (/ fact k))
                        sum (+ sum (/ (* sign term) n))
                        test (* (absv sum) eps)
                        ns (if odd [(- sign) sum sumc sumc] [sign sums sums sum])]
                    (if (< term test) [(nth ns 3) (nth ns 1)]
                        (recur (inc k) term (nth ns 2) (nth ns 1) (nth ns 3) (nth ns 0) (not odd) (+ n 2))))))))
          :else
          (let [pix2 (* Math/PI ax ax)]
            (loop [k 2 n -1 b [1.0 (- pix2)] cc [big 0.0]
                   d (cxdiv [1.0 0.0] [1.0 (- pix2)]) h (cxdiv [1.0 0.0] [1.0 (- pix2)])]
              (if (> k 100)
                (let [h (cxmul h [ax (- ax)])
                      e [(Math/cos (* 0.5 pix2)) (Math/sin (* 0.5 pix2))]
                      t (cxsub [1.0 0.0] (cxmul e h))]
                  (cxmul [0.5 0.5] t))
                (let [n (+ n 2)
                      a (* (- n) (+ n 1.0))
                      b [(+ (nth b 0) 4.0) (nth b 1)]
                      d (cxdiv [1.0 0.0] (cxadd [(* a (nth d 0)) (* a (nth d 1))] b))
                      cc (cxadd b (cxdiv [a 0.0] cc))
                      del (cxmul cc d)
                      h (cxmul h del)]
                  (if (<= (+ (absv (- (nth del 0) 1.0)) (absv (nth del 1))) eps)
                    (let [h (cxmul h [ax (- ax)])
                          e [(Math/cos (* 0.5 pix2)) (Math/sin (* 0.5 pix2))]
                          t (cxsub [1.0 0.0] (cxmul e h))]
                      (cxmul [0.5 0.5] t))
                    (recur (inc k) n b cc d h))))))))

(defn cisi [x]
  (let [euler 0.577215664901533
        piby2 1.570796326794897
        t (absv x)]
    (cond (zero? t) [(- big) 0.0]
          (> t 2.0)
          (let [h (loop [i 1 b [1.0 t] c [big 0.0]
                         d (cxdiv [1.0 0.0] [1.0 t]) h (cxdiv [1.0 0.0] [1.0 t])]
                    (if (>= i 100) h
                        (let [a (* (- i) i)
                              b [(+ (nth b 0) 2.0) (nth b 1)]
                              d (cxdiv [1.0 0.0] (cxadd [(* a (nth d 0)) (* a (nth d 1))] b))
                              c (cxadd b (cxdiv [a 0.0] c))
                              del (cxmul c d)
                              h (cxmul h del)]
                          (if (<= (+ (absv (- (nth del 0) 1.0)) (absv (nth del 1))) eps)
                            h (recur (inc i) b c d h)))))
                h (cxmul [(Math/cos t) (- (Math/sin t))] h)]
            [(- (nth h 0)) (+ (nth h 1) piby2)])
          :else
          (let [[sumc sums] (if (< t (Math/sqrt czmin)) [0.0 t]
                                (loop [k 1 fact 1.0 sum 0.0 sign 1.0 sc 0.0 ss 0.0 odd true]
                                  (if (> k 100) [sc ss]
                                      (let [fact (* fact (/ t k))
                                            term (/ fact k)
                                            sum (+ sum (* sign term))
                                            err (/ term (absv sum))
                                            ns (if odd [(- sign) sum sc sc] [sign ss ss sum])]
                                        (if (< err eps)
                                          [(nth ns 3) (nth ns 1)]
                                          (recur (inc k) fact (nth ns 2) (nth ns 0) (nth ns 3) (nth ns 1) (not odd)))))))]
            [(+ sumc (Math/log t) euler) sums]))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          x (Double/parseDouble (nth toks 1))
          [r im] (if (= op "FRESNEL")
                   (let [[cr ci] (frenel x)] (if (neg? x) [(- cr) (- ci)] [cr ci]))
                   (let [[cr ci] (cisi x)] (if (neg? x) [cr (- ci)] [cr ci])))]
      (println (fmt6 (canon r)) (fmt6 (canon im))))))
