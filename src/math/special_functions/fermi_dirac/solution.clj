;; Generalized Fermi-Dirac integral (NR 6.10).
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

(defn f-trap [kk etaa thetaa t]
  (let [e (Math/exp (- t))
        x (Math/exp (- t e))]
    (/ (* (* (* x (+ 1.0 e)) (Math/pow x kk)) (Math/sqrt (+ 1.0 (* (* thetaa 0.5) x))))
       (+ (Math/exp (- x etaa)) 1.0))))

(defn f-de [kk etaa thetaa x del]
  (let [pw (if (< x 1.0) (Math/pow del kk) (Math/pow x kk))]
    (/ (* pw (Math/sqrt (+ 1.0 (* (* thetaa 0.5) x)))) (+ (Math/exp (- x etaa)) 1.0))))

(defn trap-refine [kk etaa thetaa a b n s]
  (if (= n 1)
    (* (* 0.5 (- b a)) (+ (f-trap kk etaa thetaa a) (f-trap kk etaa thetaa b)))
    (let [it (loop [j 1 v 1] (if (< j (- n 1)) (recur (inc j) (* v 2)) v))
          tnm (double it)
          del (/ (- b a) tnm)]
      (loop [j 0 x (+ a (* 0.5 del)) acc 0.0]
        (if (>= j it)
          (* 0.5 (+ s (/ (* (- b a) acc) tnm)))
          (recur (inc j) (+ x del) (+ acc (f-trap kk etaa thetaa x))))))))

(defn de-refine [kk etaa thetaa a b hmax n s]
  (if (= n 1)
    (* (* (* (* hmax 2.0) (- b a)) 0.25)
       (f-de kk etaa thetaa (* 0.5 (+ b a)) (* 0.5 (- b a))))
    (let [it (loop [j 1 v 1] (if (< j (- n 1)) (recur (inc j) (* v 2)) v))
          twoh (/ hmax (double it))]
      (loop [j 0 t (* 0.5 twoh) acc 0.0]
        (if (>= j it)
          (+ (* 0.5 s) (* (* (- b a) twoh) acc))
          (let [q (Math/exp (* -2.0 (Math/sinh t)))
                del (/ (* (- b a) q) (+ 1.0 q))
                fact (* (/ q (* (+ 1.0 q) (+ 1.0 q))) (Math/cosh t))]
            (recur (inc j) (+ t twoh)
                   (+ acc (* fact (+ (f-de kk etaa thetaa (+ a del) del)
                                     (f-de kk etaa thetaa (- b del) del)))))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (>= (count toks) 3)
    (let [kk (Double/parseDouble (nth toks 0))
          etaa (Double/parseDouble (nth toks 1))
          thetaa (Double/parseDouble (nth toks 2))
          eps 3.0e-9
          nmax 11]
      (loop [i 1 n 0 s 0.0 n0 0 s0 0.0 n1 0 s1 0.0 olds 0.0 sum 0.0 flag 1]
        (if (> i nmax)
          (println (str (fmt6 (canon sum)) " " flag))
          (let [[nsum nn ns nn0 ns0 nn1 ns1]
                (if (<= etaa 15.0)
                  (let [nn (inc n) ns (trap-refine kk etaa thetaa -4.5 5.0 nn s)]
                    [ns nn ns n0 s0 n1 s1])
                  (let [nn0 (inc n0)
                        ns0 (de-refine kk etaa thetaa 0.0 etaa 4.3 nn0 s0)
                        nn1 (inc n1)
                        ns1 (de-refine kk etaa thetaa etaa (+ etaa 60.0) 4.3 nn1 s1)]
                    [(+ ns0 ns1) n s nn0 ns0 nn1 ns1]))]
            (if (and (> i 3) (<= (absv (- nsum olds)) (* eps (absv olds))))
              (println (str (fmt6 (canon nsum)) " " 0))
              (recur (inc i) nn ns nn0 ns0 nn1 ns1 nsum nsum flag))))))))
