;; Adaptive quadrature (NR 4.7).
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

(defn lobatto4 [fe r5 aa bb]
  (let [m (* 0.5 (+ aa bb)) h (* 0.5 (- bb aa))]
    (* h (+ (/ (+ (fe (- m h)) (fe (+ m h))) 6.0)
            (/ (* 5.0 (+ (fe (- m (* h r5))) (fe (+ m (* h r5))))) 6.0)))))

(defn kronrod7 [fe r5 r23 aa bb]
  (let [m (* 0.5 (+ aa bb)) h (* 0.5 (- bb aa))]
    (* h (+ (/ (* 11.0 (+ (fe (- m h)) (fe (+ m h)))) 210.0)
            (/ (* 72.0 (+ (fe (- m (* h r23))) (fe (+ m (* h r23))))) 245.0)
            (/ (* 125.0 (+ (fe (- m (* h r5))) (fe (+ m (* h r5))))) 294.0)
            (/ (* 16.0 (fe m)) 35.0)))))

(declare arec)

(defn arec [fe r5 r23 eps iscale aa bb depth]
  (let [i1 (kronrod7 fe r5 r23 aa bb)
        i2 (lobatto4 fe r5 aa bb)
        m (* 0.5 (+ aa bb))]
    (cond (<= (absv (- i1 i2)) (* eps (absv iscale))) [i1 false]
          (or (>= depth 12) (<= m aa) (<= bb m)) [i1 true]
          :else (let [[l fl] (arec fe r5 r23 eps iscale aa m (inc depth))
                      [r fr] (arec fe r5 r23 eps iscale m bb (inc depth))]
                  [(+ l r) (or fl fr)]))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [fun (nth toks 0)
          [p rest] (cond (= fun "POLY") (let [d (Long/parseLong (nth toks 1))]
                                          [(vec (for [i (range (inc d))] (Double/parseDouble (nth toks (+ 2 i)))))
                                           (+ 3 d)])
                       (= fun "POW") [(vec (for [i (range 4)] (Double/parseDouble (nth toks (+ 1 i))))) 5]
                       :else [(vec (for [i (range 3)] (Double/parseDouble (nth toks (+ 1 i))))) 4])
          a (Double/parseDouble (nth toks rest))
          b (Double/parseDouble (nth toks (inc rest)))
          eps (Double/parseDouble (nth toks (+ rest 2)))
          r5 (/ 1.0 (Math/sqrt 5.0))
          r23 (Math/sqrt (/ 2.0 3.0))
          fe (fn [t] (feval fun p t))
          iscale (kronrod7 fe r5 r23 a b)
          [ans oot] (arec fe r5 r23 eps iscale a b 0)]
      (println (fmt6 (canon ans)) (if oot 1 0)))))
