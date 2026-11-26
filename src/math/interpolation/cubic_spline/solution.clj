;; Cubic spline interpolation (NR 3.3).
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

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          n (Long/parseLong (nth toks 1))
          nums (mapv #(Double/parseDouble %) (subvec toks 2))
          xv (vec (subvec nums 0 n))
          yv (vec (subvec nums n (+ n n)))
          yp1 (nth nums (+ n n))
          ypn (nth nums (+ n n 1))
          [y2 u] (if (> yp1 0.99e99)
                   [(assoc (vec (repeat n 0.0)) 0 0.0)
                    (assoc (vec (repeat (dec n) 0.0)) 0 0.0)]
                   [(assoc (vec (repeat n 0.0)) 0 -0.5)
                    (assoc (vec (repeat (dec n) 0.0)) 0
                           (* (/ 3.0 (- (nth xv 1) (nth xv 0)))
                              (- (/ (- (nth yv 1) (nth yv 0)) (- (nth xv 1) (nth xv 0))) yp1)))])
          [y2 u] (loop [i 1 y2 y2 u u]
                   (if (>= i (dec n)) [y2 u]
                       (let [sig (/ (- (nth xv i) (nth xv (dec i)))
                                    (- (nth xv (inc i)) (nth xv (dec i))))
                             p (+ (* sig (nth y2 (dec i))) 2.0)
                             ui (- (/ (- (nth yv (inc i)) (nth yv i)) (- (nth xv (inc i)) (nth xv i)))
                                    (/ (- (nth yv i) (nth yv (dec i))) (- (nth xv i) (nth xv (dec i)))))]
                         (recur (inc i)
                                (assoc y2 i (/ (- sig 1.0) p))
                                (assoc u i (/ (- (/ (* 6.0 ui) (- (nth xv (inc i)) (nth xv (dec i))))
                                                  (* sig (nth u (dec i)))) p))))))
          [qn un] (if (> ypn 0.99e99) [0.0 0.0]
                      [0.5 (* (/ 3.0 (- (nth xv (dec n)) (nth xv (- n 2))))
                               (- ypn (/ (- (nth yv (dec n)) (nth yv (- n 2)))
                                         (- (nth xv (dec n)) (nth xv (- n 2))))))])
          y2 (assoc y2 (dec n) (/ (- un (* qn (nth u (- n 2))))
                                  (+ (* qn (nth y2 (- n 2))) 1.0)))
          y2 (loop [k (- n 2) y2 y2]
               (if (< k 0) y2
                   (recur (dec k) (assoc y2 k (+ (* (nth y2 k) (nth y2 (inc k))) (nth u k))))))]
      (if (= op "SECOND")
        (print (str/join " " (map #(fmt6 (canon %)) y2)))
        (let [q (long (nth nums (+ n n 2)))
              qs (subvec nums (+ n n 3) (+ (+ n n 3) q))]
          (print (str/join "\n"
                           (map (fn [x]
                                  (let [klo (loop [klo 0 khi (dec n)]
                                              (if (<= (- khi klo) 1) klo
                                                  (let [k (quot (+ khi klo) 2)]
                                                    (if (> (nth xv k) x)
                                                      (recur klo k)
                                                      (recur k khi)))))
                                        jl (min klo (- n 2))
                                        h (- (nth xv (inc jl)) (nth xv jl))
                                        a (/ (- (nth xv (inc jl)) x) h)
                                        b (/ (- x (nth xv jl)) h)
                                        y (+ (* a (nth yv jl)) (* b (nth yv (inc jl)))
                                               (/ (* (+ (* (- (* a a a) a) (nth y2 jl))
                                                        (* (- (* b b b) b) (nth y2 (inc jl))))
                                                     (* h h))
                                                  6.0))]
                                    (fmt6 (canon y))))
                                qs))))))))
