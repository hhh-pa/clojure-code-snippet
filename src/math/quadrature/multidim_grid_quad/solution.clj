;; Multidimensional grid quadrature (NR 4.8).
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

(defn feval [f3op p x y z]
  (cond (= f3op "P3") (+ (nth p 0) (* (nth p 1) x) (* (nth p 2) y) (* (nth p 3) z)
                         (* (nth p 4) x x) (* (nth p 5) y y) (* (nth p 6) z z)
                         (* (nth p 7) x y) (* (nth p 8) x z) (* (nth p 9) y z))
        (= f3op "E3") (* (nth p 0) (Math/exp (+ (* (nth p 1) x) (* (nth p 2) y) (* (nth p 3) z))))
        :else (* (nth p 0) (Math/sin (+ (* (nth p 1) x) (* (nth p 2) y) (* (nth p 3) z) (nth p 4))))))

(defn leg-root [z n eps]
  (loop [z z]
    (let [[p1 p2] (loop [j 0 p1 1.0 p2 0.0]
                    (if (>= j n) [p1 p2]
                        (let [p3 p2 p2n p1]
                          (recur (inc j) (/ (- (* (+ (* 2.0 j) 1.0) z p2n) (* j p3)) (+ j 1)) p2n))))
          pp (/ (* n (- (* z p1) p2)) (- (* z z) 1.0))
          z1 z
          zn (- z1 (/ p1 pp))]
      (if (<= (absv (- zn z1)) eps) [zn pp] (recur zn)))))

(defn gauleg10 []
  (let [n 10 m (quot (+ 10 1) 2)]
    (loop [i 0 gx (vec (repeat 10 0.0)) gw (vec (repeat 10 0.0))]
      (if (>= i m) [gx gw]
          (let [z0 (Math/cos (/ (* 3.141592654 (+ i 0.75)) (+ 10 0.5)))
                [z pp] (leg-root z0 10 1.0e-14)
                ww (/ 2.0 (* (- 1.0 (* z z)) pp pp))]
            (recur (inc i) (assoc gx i (- z) (- 10 1 i) z) (assoc gw i ww (- 10 1 i) ww)))))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [f3op (nth toks 0)
          np (cond (= f3op "P3") 10 (= f3op "E3") 4 :else 5)
          p (vec (for [i (range np)] (Double/parseDouble (nth toks (+ 1 i)))))
          pos (+ 1 np)
          x1 (Double/parseDouble (nth toks pos))
          x2 (Double/parseDouble (nth toks (+ pos 1)))
          pos (+ pos 2)
          [y1op y1p pos] (let [op (nth toks pos)]
                           (if (= op "C") [op [(Double/parseDouble (nth toks (+ pos 1)))] (+ pos 2)]
                               [op [(Double/parseDouble (nth toks (+ pos 1))) (Double/parseDouble (nth toks (+ pos 2)))] (+ pos 3)]))
          [y2op y2p pos] (let [op (nth toks pos)]
                           (if (= op "C") [op [(Double/parseDouble (nth toks (+ pos 1)))] (+ pos 2)]
                               [op [(Double/parseDouble (nth toks (+ pos 1))) (Double/parseDouble (nth toks (+ pos 2)))] (+ pos 3)]))
          [z1op z1p pos] (let [op (nth toks pos)]
                           (if (= op "C") [op [(Double/parseDouble (nth toks (+ pos 1)))] (+ pos 2)]
                               [op [(Double/parseDouble (nth toks (+ pos 1))) (Double/parseDouble (nth toks (+ pos 2))) (Double/parseDouble (nth toks (+ pos 3)))] (+ pos 4)]))
          [z2op z2p _] (let [op (nth toks pos)]
                         (if (= op "C") [op [(Double/parseDouble (nth toks (+ pos 1)))] (+ pos 2)]
                             [op [(Double/parseDouble (nth toks (+ pos 1))) (Double/parseDouble (nth toks (+ pos 2))) (Double/parseDouble (nth toks (+ pos 3)))] (+ pos 4)]))
          ylo (fn [x] (if (= y1op "C") (nth y1p 0) (+ (* (nth y1p 0) x) (nth y1p 1))))
          yhi (fn [x] (if (= y2op "C") (nth y2p 0) (+ (* (nth y2p 0) x) (nth y2p 1))))
          zlo (fn [x y] (if (= z1op "C") (nth z1p 0) (+ (nth z1p 0) (* (nth z1p 1) x) (* (nth z1p 2) y))))
          zhi (fn [x y] (if (= z2op "C") (nth z2p 0) (+ (nth z2p 0) (* (nth z2p 1) x) (* (nth z2p 2) y))))
          [gx gw] (gauleg10)
          ans (loop [i 0 ans 0.0]
                (if (>= i 10) ans
                    (let [x (+ (* 0.5 (+ x1 x2)) (* 0.5 (- x2 x1) (nth gx i)))
                          yl (ylo x) yh (yhi x)]
                      (recur (inc i)
                             (loop [j 0 ans ans]
                               (if (>= j 10) ans
                                   (let [y (+ (* 0.5 (+ yl yh)) (* 0.5 (- yh yl) (nth gx j)))
                                         zl (zlo x y) zh (zhi x y)
                                         inner (loop [k 0 s 0.0]
                                                 (if (>= k 10) s
                                                     (let [z (+ (* 0.5 (+ zl zh)) (* 0.5 (- zh zl) (nth gx k)))]
                                                       (recur (inc k) (+ s (* (nth gw k) (feval f3op p x y z)))))))]
                                     (recur (inc j) (+ ans (* (nth gw i) (nth gw j) 0.125 (- x2 x1) (- yh yl) (- zh zl) inner))))))))))]
      (println (fmt6 (canon ans))))))
