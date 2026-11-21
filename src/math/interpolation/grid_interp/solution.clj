;; Grid interpolation (NR 3.6). Port of the C++ reference.
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

(defn locate [xx x]
  (let [n (count xx)]
    (loop [klo 0 khi (dec n)]
      (if (<= (- khi klo) 1) (min klo (- n 2))
          (let [k (quot (+ khi klo) 2)]
            (if (> (nth xx k) x)
              (recur klo k)
              (recur k khi)))))))

(defn neville [xa ya m x]
  (let [[ns _] (loop [i 0 ns 0 dif (absv (- x (nth xa 0)))]
                 (if (>= i m) [ns dif]
                     (let [dift (absv (- x (nth xa i)))]
                       (if (< dift dif)
                         (recur (inc i) i dift)
                         (recur (inc i) ns dif)))))
        [y _] (loop [mm 1 y (nth ya ns) c (vec (subvec (vec ya) 0 m))
                     d (vec (subvec (vec ya) 0 m)) ns (dec ns)]
                (if (>= mm m) [y nil]
                    (let [[c d] (loop [i 0 c c d d]
                                  (if (>= i (- m mm)) [c d]
                                      (let [ho (- (nth xa i) x)
                                            hp (- (nth xa (+ i mm)) x)
                                            w (- (nth c (inc i)) (nth d i))
                                            den (/ w (- ho hp))]
                                        (recur (inc i)
                                               (assoc c i (* ho den))
                                               (assoc d i (* hp den))))))
                          [dy ns] (if (< (* 2 (inc ns)) (- m mm))
                                    [(nth c (inc ns)) ns]
                                    [(nth d ns) (dec ns)])]
                      (recur (inc mm) (+ y dy) c d ns))))]
    y))

(defn spline-y2 [xv yv]
  (let [n (count xv)
        [y2 u] (loop [i 1
                      y2 (assoc (vec (repeat n 0.0)) 0 0.0)
                      u (assoc (vec (repeat (dec n) 0.0)) 0 0.0)]
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
        y2 (assoc y2 (dec n) 0.0)]
    (loop [k (- n 2) y2 y2]
      (if (< k 0) y2
          (recur (dec k) (assoc y2 k (+ (* (nth y2 k) (nth y2 (inc k))) (nth u k))))))))

(defn splint [xv yv y2 x]
  (let [jl (locate xv x)
        h (- (nth xv (inc jl)) (nth xv jl))
        a (/ (- (nth xv (inc jl)) x) h)
        b (/ (- x (nth xv jl)) h)]
    (+ (* a (nth yv jl)) (* b (nth yv (inc jl)))
       (/ (* (+ (* (- (* a a a) a) (nth y2 jl))
                 (* (- (* b b b) b) (nth y2 (inc jl))))
              (* h h))
          6.0))))

(def WT
  [[1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0]
   [-3 0 0 3 0 0 0 0 -2 0 0 -1 0 0 0 0]
   [2 0 0 -2 0 0 0 0 1 0 0 1 0 0 0 0]
   [0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0]
   [0 0 0 0 -3 0 0 3 0 0 0 0 -2 0 0 -1]
   [0 0 0 0 2 0 0 -2 0 0 0 0 1 0 0 1]
   [-3 3 0 0 -2 -1 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 -3 3 0 0 -2 -1 0 0]
   [9 -9 9 -9 6 3 -3 -6 6 -6 -3 3 4 2 1 2]
   [-6 6 -6 6 -4 -2 2 4 -3 3 3 -3 -2 -1 -1 -2]
   [2 -2 0 0 1 1 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 2 -2 0 0 1 1 0 0]
   [-6 6 -6 6 -3 -3 3 3 -4 4 2 -2 -2 -2 -1 -1]
   [4 -4 4 -4 2 2 -2 -2 2 -2 -2 2 1 1 1 1]])

(defn bilin-point [x1v x2v ym x1p x2p]
  (let [i (locate x1v x1p) j (locate x2v x2p)
        t (/ (- x1p (nth x1v i)) (- (nth x1v (inc i)) (nth x1v i)))
        u (/ (- x2p (nth x2v j)) (- (nth x2v (inc j)) (nth x2v j)))]
    (+ (* (- 1.0 t) (- 1.0 u) (get-in ym [i j]))
       (* t (- 1.0 u) (get-in ym [(inc i) j]))
       (* (- 1.0 t) u (get-in ym [i (inc j)]))
       (* t u (get-in ym [(inc i) (inc j)])))))

(defn block-start [xv xp mm]
  (let [n (count xv)
        klo (loop [klo 0 khi (dec n)]
              (if (<= (- khi klo) 1) klo
                  (let [k (quot (+ khi klo) 2)]
                    (if (> (nth xv k) xp) (recur klo k) (recur k khi)))))]
    (max 0 (min (- n mm) (- klo (quot (- mm 2) 2))))))

(defn poly2-point [x1v x2v ym mm nn x1p x2p]
  (let [bi (block-start x1v x1p mm)
        bj (block-start x2v x2p nn)
        yv (vec (for [k (range bi (+ bi mm))]
                  (neville (vec (subvec x2v bj (+ bj nn)))
                           (vec (subvec (nth ym k) bj (+ bj nn)))
                           nn x2p)))]
    (neville (vec (subvec x1v bi (+ bi mm))) yv mm x1p)))

(defn spl2-point [x1v x2v ym rowy2 x1p x2p]
  (let [yv (mapv #(splint x2v %1 %2 x2p) ym rowy2)
        cy2 (spline-y2 x1v yv)]
    (splint x1v yv cy2 x1p)))


(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (if (= op "BICUBIC")
        (let [nums (mapv #(Double/parseDouble %) (subvec toks 1))
              x1l (nth nums 0) x1u (nth nums 1) x2l (nth nums 2) x2u (nth nums 3)
              y (vec (subvec nums 4 8)) y1 (vec (subvec nums 8 12))
              y2 (vec (subvec nums 12 16)) y12 (vec (subvec nums 16 20))
              q (long (nth nums 20))
              qs (subvec nums 21 (+ 21 (* 2 q)))
              d1 (- x1u x1l) d2 (- x2u x2l) d1d2 (* d1 d2)
              x (vec (concat y (map #(* % d1) y1) (map #(* % d2) y2) (map #(* % d1d2) y12)))
              c (vec (for [i (range 16)]
                       (reduce + 0.0 (map * (nth WT i) x))))
              c (vec (for [i (range 4)] (vec (subvec c (* i 4) (+ (* i 4) 4)))))]
          (print (str/join "\n"
                           (map (fn [k]
                                  (let [x1 (nth qs (* k 2)) x2 (nth qs (inc (* k 2)))
                                        t (/ (- x1 x1l) d1) u (/ (- x2 x2l) d2)
                                        [ansy ansy1 ansy2]
                                        (loop [i 3 a 0.0 a1 0.0 a2 0.0]
                                          (if (< i 0) [a a1 a2]
                                              (let [d0 (+ (* (get-in c [i 3]) u) (get-in c [i 2]))
                                                    e0 (+ (* d0 u) (get-in c [i 1]))
                                                    f1 (+ (* (* 3.0 (get-in c [3 i])) t) (* 2.0 (get-in c [2 i])))
                                                    f2 (+ (* (* 3.0 (get-in c [i 3])) u) (* 2.0 (get-in c [i 2])))]
                                                (recur (dec i)
                                                       (+ (* t a) (+ (* e0 u) (get-in c [i 0])))
                                                       (+ (* u a1) (+ (* f1 t) (get-in c [1 i])))
                                                       (+ (* t a2) (+ (* f2 u) (get-in c [i 1])))))))]
                                    (str (fmt6 (canon ansy)) " " (fmt6 (canon (/ ansy1 d1))) " " (fmt6 (canon (/ ansy2 d2))))))
                                (range q)))))
        (let [m (Long/parseLong (nth toks 1))
              n (Long/parseLong (nth toks 2))
              nums (mapv #(Double/parseDouble %) (subvec toks 3))
              x1v (vec (subvec nums 0 m))
              x2v (vec (subvec nums m (+ m n)))
              ym (vec (for [i (range m)] (vec (subvec nums (+ (+ m n) (* i n)) (+ (+ m n) (* (inc i) n))))))
              restv (subvec nums (+ (+ m n) (* m n)))
              [mm nn restv] (if (= op "POLY2D")
                             [(long (nth restv 0)) (long (nth restv 1)) (subvec restv 2)]
                             [0 0 restv])
              q (long (nth restv 0))
              qs (subvec restv 1 (+ 1 (* 2 q)))
              rowy2 (when (= op "SPLINE2D") (mapv #(spline-y2 x2v %) ym))]
          (print (str/join "\n"
                           (map (fn [k]
                                  (let [x1p (nth qs (* k 2)) x2p (nth qs (inc (* k 2)))]
                                    (fmt6 (canon
                                           (cond
                                             (= op "BILINEAR")
                                             (bilin-point x1v x2v ym x1p x2p)
                                             (= op "POLY2D")
                                             (poly2-point x1v x2v ym mm nn x1p x2p)
                                             :else
                                             (spl2-point x1v x2v ym rowy2 x1p x2p))))))
                                (range q)))))))))
