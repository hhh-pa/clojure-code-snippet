;; Improper integrals (NR 4.4).
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

(defn geval [op fun p ain bin x]
  (cond (= op "MIDPNT") (feval fun p x)
        (= op "MIDINF") (/ (feval fun p (/ 1.0 x)) (* x x))
        (= op "MIDEXP") (/ (feval fun p (- (Math/log x))) x)
        (= op "MIDSQL") (* 2.0 x (feval fun p (+ ain (* x x))))
        :else (* 2.0 x (feval fun p (- bin (* x x))))))

(defn mid-next [op fun p a b ain bin n s]
  (let [n (inc n)]
    (if (= n 1)
      [n (* (- b a) (geval op fun p ain bin (* 0.5 (+ a b))))]
      (let [it (loop [it 1 j 1] (if (>= j (- n 1)) it (recur (* it 3) (inc j))))
            tnm (double it)
            del (/ (- b a) (* 3.0 tnm))
            ddel (+ del del)
            sum (loop [j 0 x (+ a (* 0.5 del)) sum 0.0]
                  (if (>= j it) sum
                      (let [s1 (+ sum (geval op fun p ain bin x))
                            x1 (+ x ddel)
                            s2 (+ s1 (geval op fun p ain bin x1))]
                        (recur (inc j) (+ x1 del) s2))))]
        [n (/ (+ s (/ (* (- b a) sum) tnm)) 3.0)]))))

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
    (let [op (nth toks 0)
          fun (nth toks 1)
          [p rest] (cond (= fun "POLY") (let [d (Long/parseLong (nth toks 2))]
                                          [(vec (for [i (range (inc d))] (Double/parseDouble (nth toks (+ 3 i)))))
                                           (+ 4 d)])
                       (= fun "POW") [(vec (for [i (range 4)] (Double/parseDouble (nth toks (+ 2 i))))) 6]
                       :else [(vec (for [i (range 3)] (Double/parseDouble (nth toks (+ 2 i))))) 5])
          num (fn [t] (cond (= t "INF") 1.0e99 (= t "-INF") -1.0e99 :else (Double/parseDouble t)))
          ain (num (nth toks rest))
          bin (num (nth toks (inc rest)))
          [a b] (cond (= op "MIDINF") [(/ 1.0 bin) (/ 1.0 ain)]
                      (= op "MIDEXP") [0.0 (Math/exp (- ain))]
                      (or (= op "MIDSQL") (= op "MIDSQU")) [0.0 (Math/sqrt (- bin ain))]
                      :else [ain bin])
          eps 3.0e-9
          jmax 14
          k 5
          ans (loop [j 1 n 0 s 0.0 ss (vec (repeat jmax 0.0)) h (assoc (vec (repeat (inc jmax) 0.0)) 0 1.0) ans 0.0]
                (if (> j jmax) ans
                    (let [[n s] (mid-next op fun p a b ain bin n s)
                          ss (assoc ss (dec j) s)]
                      (if (< j k)
                        (recur (inc j) n s ss (assoc h j (/ (nth h (dec j)) 9.0)) ans)
                        (let [[y dy] (rawinterp h ss j k)]
                          (if (<= (absv dy) (* eps (absv y))) y
                              (recur (inc j) n s ss (assoc h j (/ (nth h (dec j)) 9.0)) y)))))))]
      (println (fmt6 (canon ans))))))
