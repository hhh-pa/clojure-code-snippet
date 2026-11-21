;; Quadrature by variable transformation (NR 4.5). Port of the C++ reference.
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

(defn feval [fun p t]
  (cond (= fun "POLY") (reduce (fn [s c] (+ (* s t) c)) 0.0 (reverse p))
        (= fun "EXP") (+ (* (nth p 0) (Math/exp (* (nth p 1) t))) (nth p 2))
        (= fun "TRIG") (* (nth p 0) (Math/sin (+ (* (nth p 1) t) (nth p 2))))
        (= fun "LOG") (* (nth p 0) (Math/log (+ (* (nth p 1) t) (nth p 2))))
        (= fun "GAUSS") (let [z (/ (- t (nth p 1)) (nth p 2))] (* (nth p 0) (Math/exp (- (* z z)))))
        :else (* (nth p 0) (Math/pow (+ (* (nth p 1) t) (nth p 2)) (nth p 3)))))

(defn txrule [rule t]
  (cond (= rule "SEMIINF_DE") (let [x (Math/exp (* Math/PI (Math/sinh t)))]
                                [x (* x Math/PI (Math/cosh t))])
        (= rule "SEMIINF_TANH") (let [x (Math/exp t)] [x x])
        (= rule "INF_TANH") [(Math/sinh t) (Math/cosh t)]
        :else (let [x (Math/exp (- t (Math/exp (- t))))]
                [x (* x (+ 1.0 (Math/exp (- t))))])))

(defn traprange [rule fun p lo hi l sgn]
  (let [g (fn [t] (let [[x dxdt] (txrule rule t)] (* (feval fun p (* sgn x)) dxdt)))]
    (loop [m 0 n 0 s 0.0]
      (if (>= m l) s
          (let [n (inc n)]
            (if (= n 1)
              (recur (inc m) n (* 0.5 (- hi lo) (+ (g lo) (g hi))))
              (let [it (loop [it 1 j 1] (if (>= j (- n 1)) it (recur (* it 2) (inc j))))
                    tnm (double it)
                    del (/ (- hi lo) tnm)
                    sum (loop [j 0 x (+ lo (* 0.5 del)) sum 0.0]
                          (if (>= j it) sum (recur (inc j) (+ x del) (+ sum (g x)))))]
                (recur (inc m) n (* 0.5 (+ s (/ (* (- hi lo) sum) tnm)))))))))))

(defn definte [fun p a b hmax l]
  (loop [n 1 s 0.0]
    (if (> n l) s
        (if (= n 1)
          (recur (inc n) (* hmax 2.0 (- b a) 0.25 (feval fun p (* 0.5 (+ b a)))))
          (let [it (loop [it 1 j 1] (if (>= j (- n 1)) it (recur (* it 2) (inc j))))
                twoh (/ hmax it)
                sum (loop [j 0 t (* 0.5 twoh) sum 0.0]
                      (if (>= j it) sum
                          (let [q (Math/exp (* -2.0 (Math/sinh t)))
                                del (/ (* (- b a) q) (+ 1.0 q))
                                fact (* (/ q (* (+ 1.0 q) (+ 1.0 q))) (Math/cosh t))]
                            (recur (inc j) (+ t twoh)
                                   (+ sum (* fact (+ (feval fun p (+ a del)) (feval fun p (- b del)))))))))]
            (recur (inc n) (+ (* 0.5 s) (* (- b a) twoh sum))))))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          fun (nth toks 1)
          [p rest] (cond (= fun "POLY") (let [d (Long/parseLong (nth toks 2))]
                                          [(vec (for [i (range (inc d))] (Double/parseDouble (nth toks (+ 3 i)))))
                                           (+ 4 d)])
                       (= fun "POW") [(vec (for [i (range 4)] (Double/parseDouble (nth toks (+ 2 i))))) 6]
                       :else [(vec (for [i (range 3)] (Double/parseDouble (nth toks (+ 2 i))))) 5])
          ans (if (= op "DE_FINITE")
                (let [a (Double/parseDouble (nth toks rest))
                      b (Double/parseDouble (nth toks (inc rest)))
                      hmax (Double/parseDouble (nth toks (+ rest 2)))
                      l (Long/parseLong (nth toks (+ rest 3)))]
                  (definte fun p a b hmax l))
                (let [t0 (Double/parseDouble (nth toks rest))
                      t1 (Double/parseDouble (nth toks (inc rest)))
                      l (Long/parseLong (nth toks (+ rest 2)))]
                  (if (= op "INF_MIXED")
                    (+ (traprange "SEMIINF_MIXED" fun p t0 t1 l 1.0)
                       (traprange "SEMIINF_MIXED" fun p t0 t1 l -1.0))
                    (traprange op fun p t0 t1 l 1.0))))]
      (println (fmt6 (canon ans))))))
