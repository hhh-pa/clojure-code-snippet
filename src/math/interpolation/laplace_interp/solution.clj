;; Laplace interpolation (NR 3.8). Port of the C++ reference.
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

(defn cellval [nx ny i j dg x]
  (let [s0 (* dg (nth x (+ (* i ny) j)))
        s1 (if (> i 0) (- s0 (nth x (+ (* (dec i) ny) j))) s0)
        s2 (if (< i (dec nx)) (- s1 (nth x (+ (* (inc i) ny) j))) s1)
        s3 (if (> j 0) (- s2 (nth x (+ (* i ny) (dec j)))) s2)
        s4 (if (< j (dec ny)) (- s3 (nth x (+ (* i ny) (inc j)))) s3)]
    s4))

(defn scatter [y x p ng ny ci cj w]
  (let [i (nth ci p)
        j (nth cj p)
        lam (nth x (+ ng p))
        a (+ (* i ny) j)
        b (+ (* (inc i) ny) j)
        c (+ (* i ny) (inc j))
        d (+ (* (inc i) ny) (inc j))
        w0 (get-in w [p 0])
        w1 (get-in w [p 1])
        w2 (get-in w [p 2])
        w3 (get-in w [p 3])
        y (assoc y a (+ (nth y a) (* w0 lam)))
        y (assoc y b (+ (nth y b) (* w1 lam)))
        y (assoc y c (+ (nth y c) (* w2 lam)))
        y (assoc y d (+ (nth y d) (* w3 lam)))]
    (assoc y (+ ng p) (+ (* w0 (nth x a)) (* w1 (nth x b))
                          (* w2 (nth x c)) (* w3 (nth x d))))))

(defn matvec [nx ny k ng ci cj w diag x]
  (let [g (vec (for [i (range nx) j (range ny)]
                 (cellval nx ny i j (nth diag (+ (* i ny) j)) x)))
        y (into g (repeat k 0.0))]
    (reduce (fn [y p] (scatter y x p ng ny ci cj w)) y (range k))))

(defn vdot [a b n]
  (loop [t 0 s 0.0]
    (if (>= t n) s (recur (inc t) (+ s (* (nth a t) (nth b t)))))))

(defn bicg [nx ny k ng n ci cj w diag pz]
  (let [x0 (into (vec (repeat ng 0.0)) pz)
        q0 (matvec nx ny k ng ci cj w diag x0)
        r0 (vec (for [t (range n)]
                  (- (if (< t ng) 0.0 (nth pz (- t ng))) (nth q0 t))))]
    (loop [it 0 x x0 r r0 rt r0 pp (vec (repeat n 0.0)) pt (vec (repeat n 0.0)) rho 1.0]
      (if (>= it 400) x
          (let [z (vec (for [t (range n)] (/ (nth r t) (nth diag t))))
                zt (vec (for [t (range n)] (/ (nth rt t) (nth diag t))))
                rhon (vdot rt z n)
                beta (if (zero? rho) 0.0 (/ rhon rho))
                pp2 (vec (for [t (range n)] (+ (nth z t) (* beta (nth pp t)))))
                pt2 (vec (for [t (range n)] (+ (nth zt t) (* beta (nth pt t)))))
                q (matvec nx ny k ng ci cj w diag pp2)
                qt (matvec nx ny k ng ci cj w diag pt2)
                sig (vdot pt2 q n)
                alpha (if (zero? sig) 0.0 (/ rhon sig))
                x2 (vec (for [t (range n)] (+ (nth x t) (* alpha (nth pp2 t)))))
                r2 (vec (for [t (range n)] (- (nth r t) (* alpha (nth q t)))))
                rt2 (vec (for [t (range n)] (- (nth rt t) (* alpha (nth qt t)))))]
            (recur (inc it) x2 r2 rt2 pp2 pt2 rhon))))))

(defn clamp01 [v] (if (< v 0.0) 0.0 (if (> v 1.0) 1.0 v)))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [nx (Long/parseLong (nth toks 0))
          ny (Long/parseLong (nth toks 1))
          x0 (Double/parseDouble (nth toks 2))
          x1 (Double/parseDouble (nth toks 3))
          y0 (Double/parseDouble (nth toks 4))
          y1 (Double/parseDouble (nth toks 5))
          k (Long/parseLong (nth toks 6))
          nums (mapv #(Double/parseDouble %) (subvec toks 7))
          px (vec (for [p (range k)] (nth nums (* p 3))))
          py (vec (for [p (range k)] (nth nums (+ (* p 3) 1))))
          pz (vec (for [p (range k)] (nth nums (+ (* p 3) 2))))
          dx (/ (- x1 x0) (- nx 1))
          dy (/ (- y1 y0) (- ny 1))
          cells (vec (for [p (range k)]
                        (let [fx0 (/ (- (nth px p) x0) dx)
                              fy0 (/ (- (nth py p) y0) dy)
                              fx (if (< fx0 0.0) 0.0 (if (> fx0 (- nx 1)) (double (- nx 1)) fx0))
                              fy (if (< fy0 0.0) 0.0 (if (> fy0 (- ny 1)) (double (- ny 1)) fy0))
                              i (min (long fx) (- nx 2))
                              j (min (long fy) (- ny 2))
                              tx (clamp01 (- fx i))
                              ty (clamp01 (- fy j))]
                          [i j (* (- 1.0 tx) (- 1.0 ty)) (* tx (- 1.0 ty))
                           (* (- 1.0 tx) ty) (* tx ty)])))
          ci (mapv #(nth % 0) cells)
          cj (mapv #(nth % 1) cells)
          w (mapv #(vec (subvec % 2)) cells)
          ng (* nx ny)
          n (+ ng k)
          dg (vec (for [i (range nx) j (range ny)]
                    (double (+ (if (> i 0) 1 0) (if (< i (dec nx)) 1 0)
                               (if (> j 0) 1 0) (if (< j (dec ny)) 1 0)))))
          diag (into dg (repeat k 1.0))
          x (bicg nx ny k ng n ci cj w diag pz)]
      (doseq [i (range nx)]
        (println (str/join " " (for [j (range ny)]
                                 (fmt6 (canon (nth x (+ (* i ny) j)))))))))))
