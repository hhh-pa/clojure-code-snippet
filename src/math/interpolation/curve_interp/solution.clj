;; Curve interpolation in multidimensions (NR 3.7.5). Port of the C++ reference.
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

(defn frad [a b dim]
  (Math/sqrt (reduce + 0.0 (map (fn [i] (let [d (- (nth a i) (nth b i))] (* d d))) (range dim)))))

(defn fprime [x y pm]
  (let [e (if (= pm 1) 0 (dec (count x)))
        s1 (- (nth x e) (nth x (+ e pm)))
        s2 (- (nth x e) (nth x (+ e (* pm 2))))
        s3 (- (nth x e) (nth x (+ e (* pm 3))))
        s12 (- s1 s2) s13 (- s1 s3) s23 (- s2 s3)]
    (+ (- (* (/ (* s1 s2) (* s13 s23 s3)) (nth y (+ e (* pm 3)))))
       (* (/ (* s1 s3) (* s12 s2 s23)) (nth y (+ e (* pm 2))))
       (- (* (/ (* s2 s3) (* s1 s12 s13)) (nth y (+ e pm))))
       (* (+ (/ 1.0 s1) (/ 1.0 s2) (/ 1.0 s3)) (nth y e)))))

(defn spline-forward [sv yv nn y2 u]
  (loop [i 1 y2 y2 u u]
    (if (>= i (dec nn)) [y2 u]
        (let [sig (/ (- (nth sv i) (nth sv (dec i))) (- (nth sv (inc i)) (nth sv (dec i))))
              pp (+ (* sig (nth y2 (dec i))) 2.0)
              aa (/ (- (nth yv (inc i)) (nth yv i)) (- (nth sv (inc i)) (nth sv i)))
              bb (/ (- (nth yv i) (nth yv (dec i))) (- (nth sv i) (nth sv (dec i))))
              ny (/ (- sig 1.0) pp)
              nu (/ (- (/ (* 6.0 (- aa bb)) (- (nth sv (inc i)) (nth sv (dec i))))
                       (* sig (nth u (dec i)))) pp)]
          (recur (inc i) (assoc y2 i ny) (assoc u i nu))))))

(defn spline-y2c [sv yv db de]
  (let [nn (count sv)
        [y2 u] (if (> db 0.99e99)
                 [(assoc (vec (repeat nn 0.0)) 0 0.0) (assoc (vec (repeat (dec nn) 0.0)) 0 0.0)]
                 [(assoc (vec (repeat nn 0.0)) 0 -0.5)
                  (assoc (vec (repeat (dec nn) 0.0)) 0
                         (* (/ 3.0 (- (nth sv 1) (nth sv 0)))
                            (- (/ (- (nth yv 1) (nth yv 0)) (- (nth sv 1) (nth sv 0))) db)))])
        [y2 u] (spline-forward sv yv nn y2 u)
        [qn un] (if (> de 0.99e99) [0.0 0.0]
                    [0.5 (* (/ 3.0 (- (nth sv (dec nn)) (nth sv (- nn 2))))
                             (- de (/ (- (nth yv (dec nn)) (nth yv (- nn 2)))
                                      (- (nth sv (dec nn)) (nth sv (- nn 2))))))])
        y2 (assoc y2 (dec nn) (/ (- un (* qn (nth u (- nn 2)))) (+ (* qn (nth y2 (- nn 2))) 1.0)))]
    (loop [k (- nn 2) y2 y2]
      (if (< k 0) y2
          (recur (dec k) (assoc y2 k (+ (* (nth y2 k) (nth y2 (inc k))) (nth u k))))))))

(defn wrap01 [t]
  (let [fl (long t) f (- t fl)]
    (if (and (neg? t) (not (zero? f))) (+ f 1.0) f)))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [cls (= (nth toks 0) "CLOSED")
          n (Long/parseLong (nth toks 1))
          dim (Long/parseLong (nth toks 2))
          nums (mapv #(Double/parseDouble %) (subvec toks 3))
          ptsin (vec (for [i (range n)] (vec (subvec nums (* i dim) (+ (* i dim) dim)))))
          q (long (nth nums (* n dim)))
          qs (vec (subvec nums (+ (* n dim) 1) (+ (+ (* n dim) 1) q)))
          inn (if cls (* 2 n) n)
          ofs (if cls (quot n 2) 0)
          pts (vec (for [j (range dim)]
                     (vec (for [i (range inn)] (get-in ptsin [(mod (- i ofs) n) j])))))
          sv (loop [i 0 sv (vec (repeat inn 0.0))]
               (if (>= i inn) sv
                   (recur (inc i) (assoc sv i (if (zero? i) 0.0
                                                               (+ (nth sv (dec i))
                                                                  (frad (nth ptsin (mod (- i ofs) n)) (nth ptsin (mod (- (mod (- i ofs) n) 1) n)) dim)))))))
          ss (if cls (- (nth sv (+ ofs n)) (nth sv ofs)) (- (nth sv (dec n)) (nth sv 0)))
          soff (nth sv ofs)
          sv (mapv #(/ (- % soff) ss) sv)
          y2m (mapv (fn [j] (spline-y2c sv (nth pts j)
                                        (if (< inn 4) 1.0e99 (fprime sv (nth pts j) 1))
                                        (if (< inn 4) 1.0e99 (fprime sv (nth pts j) -1))))
                    (range dim))]
      (print (str/join "\n"
                       (map (fn [t]
                              (let [t (if cls (wrap01 t) t)]
                                (str/join " " (map (fn [j]
                                                     (let [klo (loop [klo 0 khi (dec inn)]
                                                                 (if (<= (- khi klo) 1) klo
                                                                     (let [k (quot (+ khi klo) 2)]
                                                                       (if (> (nth sv k) t)
                                                                         (recur klo k) (recur k khi)))))
                                                           jl (min klo (- inn 2))
                                                           h (- (nth sv (inc jl)) (nth sv jl))
                                                           a (/ (- (nth sv (inc jl)) t) h)
                                                           b (/ (- t (nth sv jl)) h)]
                                                       (fmt6 (canon (+ (* a (get-in pts [j jl])) (* b (get-in pts [j (inc jl)]))
                                                                       (/ (* (+ (* (- (* a a a) a) (get-in y2m [j jl]))
                                                                                (* (- (* b b b) b) (get-in y2m [j (inc jl)])))
                                                                              (* h h))
                                                                          6.0))))))
                                                   (range dim)))))
                            qs))))))
