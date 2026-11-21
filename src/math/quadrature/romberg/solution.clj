;; Romberg integration (NR 4.3). Port of the C++ reference.
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

(defn rawinterp [h ss j k]
  (let [base (- j k)
        [ns c d] (loop [i 0 ns 0 dif (absv (- (nth h base))) c (vec (repeat k 0.0)) d (vec (repeat k 0.0))]
                   (if (>= i k) [ns c d]
                       (let [dift (absv (- (nth h (+ base i))))]
                         (recur (inc i) (if (< dift dif) i ns) (if (< dift dif) dift dif)
                                (assoc c i (nth ss (+ base i))) (assoc d i (nth ss (+ base i)))))))
        y (nth ss (+ base ns))]
    (loop [mm 1 ns (dec ns) y y dy 0.0 c c d d]
      (if (>= mm k) [y dy]
          (let [[c d] (loop [i 0 c c d d]
                        (if (>= i (- k mm)) [c d]
                            (let [ho (nth h (+ base i))
                                  hp (nth h (+ base i mm))
                                  den (/ (- (nth c (inc i)) (nth d i)) (- ho hp))]
                              (recur (inc i) (assoc c i (* ho den)) (assoc d i (* hp den))))))
                [dy ns] (if (< (* 2 (inc ns)) (- k mm)) [(nth c (inc ns)) ns]
                            [(nth d ns) (dec ns)])]
            (recur (inc mm) ns (+ y dy) dy c d))))))

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
          eps 1.0e-10
          jmax 20
          k 5
          ans (loop [j 1 n 0 s 0.0 ss (vec (repeat jmax 0.0)) h (assoc (vec (repeat (inc jmax) 0.0)) 0 1.0) ans 0.0]
                (if (> j jmax) ans
                    (let [[n s] (trap-next fun p a b n s)
                          ss (assoc ss (dec j) s)]
                      (if (< j k)
                        (recur (inc j) n s ss (assoc h j (* 0.25 (nth h (dec j)))) ans)
                        (let [[y dy] (rawinterp h ss j k)]
                          (if (<= (absv dy) (* eps (absv y))) y
                              (recur (inc j) n s ss (assoc h j (* 0.25 (nth h (dec j)))) y)))))))]
      (println (fmt6 (canon ans))))))
