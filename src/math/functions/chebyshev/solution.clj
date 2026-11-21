;; Chebyshev approximation (NR 5.8-5.11). Port of the C++ reference.
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

(defn fmtv [v] (fmt6 (canon v)))

(defn chebev [x c]
  (let [m (count c)]
    (loop [j (dec m) d 0.0 dd 0.0]
      (if (< j 1) (+ (* x d) (- dd) (* 0.5 (nth c 0)))
          (recur (dec j) (+ (* 2.0 x d) (- dd) (nth c j)) d)))))

(defn feval [fun p t]
  (cond (= fun "POLY") (reduce (fn [s c] (+ (* s t) c)) 0.0 (reverse p))
        (= fun "EXP") (+ (* (nth p 0) (Math/exp (* (nth p 1) t))) (nth p 2))
        (= fun "TRIG") (* (nth p 0) (Math/sin (+ (* (nth p 1) t) (nth p 2))))
        :else (* (nth p 0) (Math/log (+ (* (nth p 1) t) (nth p 2))))))

(defn op-fit [fun p a b n]
  (vec (for [j (range n)]
         (let [s (loop [k 0 s 0.0]
                   (if (>= k n) s
                       (let [y (Math/cos (/ (* Math/PI (+ k 0.5)) n))
                             t (* 0.5 (+ (* (- b a) y) (+ a b)))]
                         (recur (inc k) (+ s (* (feval fun p t)
                                                  (Math/cos (/ (* Math/PI j (+ k 0.5)) n))))))))]
           (/ (* 2.0 s) n)))))

(defn op-deriv [c a b n]
  (let [cd (assoc (vec (repeat n 0.0)) (dec n) 0.0)
        cd (if (>= n 2) (assoc cd (- n 2) (* 2.0 (- n 1) (nth c (dec n)))) cd)
        cd (loop [j (- n 2) cd cd]
             (if (< j 1) cd
                 (recur (dec j) (assoc cd (dec j) (+ (nth cd (inc j)) (* 2.0 j (nth c j)))))))
        con (/ 2.0 (- b a))]
    (mapv #(* % con) cd)))

(defn op-integ [c a b n]
  (let [con (* 0.25 (- b a))]
    (loop [j 1 ci (vec (repeat n 0.0)) sum 0.0 fac 1.0]
      (if (> j (- n 2)) [ci sum fac]
          (let [nv (/ (* con (- (nth c (dec j)) (nth c (inc j)))) j)]
            (recur (inc j) (assoc ci j nv) (+ sum (* fac nv)) (- fac)))))))

(defn op-integ-full [c a b n]
  (let [con (* 0.25 (- b a))
        [ci sum fac] (op-integ c a b n)
        lastv (/ (* con (nth c (- n 2))) (- n 1))
        ci (assoc ci (dec n) lastv)
        sum (+ sum (* fac lastv))]
    (assoc ci 0 (* 2.0 sum))))

(defn op-topoly [c n]
  (let [[d dd] (loop [j (- n 2) d (assoc (vec (repeat n 0.0)) 0 (nth c (dec n)))
                      dd (vec (repeat n 0.0))]
                 (if (< j 1) [d dd]
                     (let [[d dd] (loop [k (- n j) d d dd dd]
                                    (if (< k 1) [d dd]
                                        (recur (dec k)
                                               (assoc d k (- (* 2.0 (nth d (dec k))) (nth dd k)))
                                               (assoc dd k (nth d k)))))]
                       (recur (dec j) (assoc d 0 (+ (- (nth dd 0)) (nth c j)))
                              (assoc dd 0 (nth d 0))))))
        d (loop [j (dec n) d d]
            (if (< j 1) d (recur (dec j) (assoc d j (- (nth d (dec j)) (nth dd j))))))]
    (assoc d 0 (+ (- (nth dd 0)) (* 0.5 (nth c 0))))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (cond
        (= op "FIT")
        (let [fun (nth toks 1)
              [p rest] (if (= fun "POLY")
                         (let [d (Long/parseLong (nth toks 2))]
                           [(vec (for [i (range (inc d))] (Double/parseDouble (nth toks (+ 3 i)))))
                            (+ 4 d)])
                         [(vec (for [i (range 3)] (Double/parseDouble (nth toks (+ 2 i))))) 5])
              a (Double/parseDouble (nth toks rest))
              b (Double/parseDouble (nth toks (inc rest)))
              n (Long/parseLong (nth toks (+ rest 2)))]
          (println (str/join " " (map fmtv (op-fit fun p a b n)))))
        (= op "EVAL")
        (let [n (Long/parseLong (nth toks 1))
              x (Double/parseDouble (nth toks 2))
              c (vec (for [i (range n)] (Double/parseDouble (nth toks (+ 3 i)))))]
          (println (fmtv (chebev x c))))
        (= op "DERIV")
        (let [n (Long/parseLong (nth toks 1))
              a (Double/parseDouble (nth toks 2))
              b (Double/parseDouble (nth toks 3))
              c (vec (for [i (range n)] (Double/parseDouble (nth toks (+ 4 i)))))]
          (println (str/join " " (map fmtv (op-deriv c a b n)))))
        (= op "INTEG")
        (let [n (Long/parseLong (nth toks 1))
              a (Double/parseDouble (nth toks 2))
              b (Double/parseDouble (nth toks 3))
              c (vec (for [i (range n)] (Double/parseDouble (nth toks (+ 4 i)))))]
          (println (str/join " " (map fmtv (op-integ-full c a b n)))))
        :else
        (let [n (Long/parseLong (nth toks 1))
              c (vec (for [i (range n)] (Double/parseDouble (nth toks (+ 2 i)))))]
          (println (str/join " " (map fmtv (op-topoly c n)))))))))
