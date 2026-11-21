;; Radial basis function interpolation (NR 3.7). Port of the C++ reference.
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
          r0 (Double/parseDouble (nth toks 1))
          normflag (not (zero? (Double/parseDouble (nth toks 2))))
          n (Long/parseLong (nth toks 3))
          dim (Long/parseLong (nth toks 4))
          nums (mapv #(Double/parseDouble %) (subvec toks 5))
          pts (vec (for [i (range n)] (vec (subvec nums (* i dim) (+ (* i dim) dim)))))
          vals (vec (subvec nums (* n dim) (+ (* n dim) n)))
          q (long (nth nums (+ (* n dim) n)))
          qs (vec (for [i (range q)] (vec (subvec nums (+ (+ (* n dim) n 1) (* i dim)) (+ (+ (* n dim) n 1) (* (inc i) dim))))))
          phi (fn [r]
                (cond (= op "MULTIQUADRIC") (Math/sqrt (+ (* r r) (* r0 r0)))
                      (= op "THINPLATE") (if (<= r 0.0) 0.0 (* (* r r) (Math/log (/ r r0))))
                      (= op "GAUSS") (let [t (/ r r0)] (Math/exp (* (* -0.5 t) t)))
                      :else (/ 1.0 (Math/sqrt (+ (* r r) (* r0 r0))))))
          rad (fn [a b] (Math/sqrt (reduce + 0.0 (map (fn [i] (let [d (- (nth a i) (nth b i))] (* d d))) (range dim)))))
          [phim rhs] (loop [i 0 phim (vec (repeat n (vec (repeat n 0.0)))) rhs (vec (repeat n 0.0))]
                       (if (>= i n) [phim rhs]
                           (let [row (mapv #(phi (rad (nth pts i) (nth pts %))) (range n))
                                 sum (reduce + 0.0 row)]
                             (recur (inc i) (assoc phim i row)
                                    (assoc rhs i (if normflag (* sum (nth vals i)) (nth vals i)))))))
          [phim p] (loop [k 0 phim phim p (vec (range n))]
                     (if (>= k n) [phim p]
                         (let [piv (loop [i (inc k) piv k]
                                     (if (>= i n) piv
                                         (if (> (absv (get-in phim [i k])) (absv (get-in phim [piv k])))
                                           (recur (inc i) i)
                                           (recur (inc i) piv))))
                               phim (if (not= piv k)
                                      (let [t (nth phim k)] (assoc (assoc phim k (nth phim piv)) piv t))
                                      phim)
                               p (if (not= piv k)
                                   (let [t (nth p k)] (assoc (assoc p k (nth p piv)) piv t))
                                   p)]
                           (recur (inc k)
                                  (loop [i (inc k) phim phim]
                                    (if (>= i n) phim
                                        (let [phim (assoc-in phim [i k] (/ (get-in phim [i k]) (get-in phim [k k])))]
                                          (recur (inc i) (loop [j (inc k) phim phim]
                                                             (if (>= j n) phim
                                                                 (recur (inc j) (assoc-in phim [i j]
                                                                                            (- (get-in phim [i j])
                                                                                               (* (get-in phim [i k]) (get-in phim [k j])))))))))))
                                  p))))
          y (loop [i 0 y (vec (repeat n 0.0))]
              (if (>= i n) y
                  (recur (inc i) (assoc y i (loop [j 0 s (nth rhs (nth p i))]
                                                     (if (>= j i) s
                                                         (recur (inc j) (- s (* (get-in phim [i j]) (nth y j))))))))))
          w (loop [i (dec n) w (vec (repeat n 0.0))]
              (if (< i 0) w
                  (recur (dec i) (assoc w i (/ (loop [j (inc i) s (nth y i)]
                                                       (if (>= j n) s
                                                           (recur (inc j) (- s (* (get-in phim [i j]) (nth w j))))))
                                                 (get-in phim [i i]))))))]
      (print (str/join "\n"
                       (map (fn [pt]
                              (let [[sumw sum] (reduce (fn [[sumw sum] i]
                                                        (let [f (phi (rad pt (nth pts i)))]
                                                          [(+ sumw (* (nth w i) f)) (+ sum f)]))
                                                      [0.0 0.0] (range n))]
                                (fmt6 (canon (if normflag (/ sumw sum) sumw)))))
                            qs))))))
