;; Numerical derivative (NR 5.7, Ridders' method). Port of the C++ reference.
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
        :else (* (nth p 0) (Math/log (+ (* (nth p 1) t) (nth p 2))))))

(defn inner-row [a i con2 err ans]
  (loop [j 1 fac con2 a a err err ans ans]
    (if (> j i) [a err ans]
        (let [nv (/ (- (* (get-in a [(dec j) i]) fac) (get-in a [(dec j) (dec i)])) (- fac 1.0))
              a (assoc-in a [j i] nv)
              fac (* fac con2)
              e1 (absv (- nv (get-in a [(dec j) i])))
              e2 (absv (- nv (get-in a [(dec j) (dec i)])))
              errt (if (> e2 e1) e2 e1)]
          (if (<= errt err)
            (recur (inc j) fac a errt nv)
            (recur (inc j) fac a err ans))))))

(defn ridders [fun p x h0]
  (let [con 1.4
        con2 (* con con)
        a0 (/ (- (feval fun p (+ x h0)) (feval fun p (- x h0))) (* 2.0 h0))]
    (loop [i 1 h h0 a (assoc-in (vec (repeat 10 (vec (repeat 10 0.0)))) [0 0] a0)
           err 1.0e30 ans a0]
      (if (>= i 10) [ans err]
          (let [h (/ h con)
                a (assoc-in a [0 i] (/ (- (feval fun p (+ x h)) (feval fun p (- x h))) (* 2.0 h)))
                [a err ans] (inner-row a i con2 err ans)]
            (recur (inc i) h a err ans))))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [fun (nth toks 0)
          [p rest] (if (= fun "POLY")
                     (let [d (Long/parseLong (nth toks 1))]
                       [(vec (for [i (range (inc d))] (Double/parseDouble (nth toks (+ 2 i)))))
                        (+ 3 d)])
                     [(vec (for [i (range 3)] (Double/parseDouble (nth toks (+ 1 i))))) 4])
          x (Double/parseDouble (nth toks rest))
          h (Double/parseDouble (nth toks (inc rest)))
          [ans err] (ridders fun p x h)]
      (println (fmt6 (canon ans)) (fmt6 (canon err))))))
