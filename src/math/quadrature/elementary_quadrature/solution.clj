;; Elementary quadrature (NR 4.2). Port of the C++ reference.
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
        :else (* (nth p 0) (Math/pow (+ (* (nth p 1) t) (nth p 2)) (nth p 3)))))

(defn trap-next [fun p a b n s]
  (let [n (inc n)]
    (if (= n 1)
      [n (* 0.5 (- b a) (+ (feval fun p a) (feval fun p b)))]
      (let [it (loop [it 1 j 1] (if (>= j (- n 1)) it (recur (* it 2) (inc j))))
            tnm (double it)
            del (/ (- b a) tnm)
            sum (loop [j 0 x (+ a (* 0.5 del)) sum 0.0]
                  (if (>= j it) sum (recur (inc j) (+ x del) (+ sum (feval fun p x)))))]
        [n (* 0.5 (+ s (/ (* (- b a) sum) tnm)))]))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          fun (nth toks 1)
          [p rest] (cond (= fun "POLY") (let [d (Long/parseLong (nth toks 2))]
                                          [(vec (for [i (range (inc d))] (Double/parseDouble (nth toks (+ 3 i)))))
                                           (+ 4 d)])
                       (= fun "POW") [(vec (for [i (range 4)] (Double/parseDouble (nth toks (+ 2 i))))) 6]
                       :else [(vec (for [i (range 3)] (Double/parseDouble (nth toks (+ 2 i))))) 5])
          a (Double/parseDouble (nth toks rest))
          b (Double/parseDouble (nth toks (inc rest)))
          eps 1.0e-10
          jmax 20
          ans (cond
                (= op "TRAPZD")
                (let [l (Long/parseLong (nth toks (+ rest 2)))]
                  (loop [j 0 n 0 s 0.0 ans 0.0]
                    (if (>= j l) ans
                        (let [[n s] (trap-next fun p a b n s)]
                          (recur (inc j) n s s)))))
                (= op "QTRAP")
                (loop [j 0 n 0 s 0.0 ans 0.0 olds 0.0]
                  (if (>= j jmax) ans
                      (let [[n s] (trap-next fun p a b n s)
                            ans s]
                        (if (and (> j 5) (or (< (absv (- ans olds)) (* eps (absv olds)))
                                               (and (zero? ans) (zero? olds))))
                          ans
                          (recur (inc j) n s ans ans)))))
                :else
                (loop [j 0 n 0 s 0.0 ans 0.0 ost 0.0 os 0.0]
                  (if (>= j jmax) ans
                      (let [[n s] (trap-next fun p a b n s)
                            st s
                            ans (/ (- (* 4.0 st) ost) 3.0)]
                        (if (and (> j 5) (or (< (absv (- ans os)) (* eps (absv os)))
                                               (and (zero? ans) (zero? os))))
                          ans
                          (recur (inc j) n s ans st ans))))))]
      (println (fmt6 (canon ans))))))
