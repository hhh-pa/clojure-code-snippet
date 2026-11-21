;; Kriging interpolation (NR 3.7.6). Port of the C++ reference.
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

(defn rdist [a b dim]
  (Math/sqrt (reduce + 0.0 (map (fn [i] (let [d (- (nth a i) (nth b i))] (* d d))) (range dim)))))

(defn lusolve [lu p rhs]
  (let [m (count rhs)
        y (reduce (fn [y i]
                    (assoc y i (reduce (fn [s j] (- s (* (get-in lu [i j]) (nth y j))))
                                       (nth rhs (nth p i)) (range i))))
                  (vec (repeat m 0.0)) (range m))]
    (reduce (fn [x i]
              (assoc x i (/ (reduce (fn [s j] (- s (* (get-in lu [i j]) (nth x j))))
                                     (nth y i) (range (inc i) m))
                            (get-in lu [i i]))))
            (vec (repeat m 0.0)) (range (dec m) -1 -1))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [beta (Double/parseDouble (nth toks 0))
          nug (Double/parseDouble (nth toks 1))
          n (Long/parseLong (nth toks 2))
          dim (Long/parseLong (nth toks 3))
          nums (mapv #(Double/parseDouble %) (subvec toks 4))
          pts (vec (for [i (range n)] (vec (subvec nums (* i dim) (+ (* i dim) dim)))))
          vals (vec (subvec nums (* n dim) (+ (* n dim) n)))
          q (long (nth nums (+ (* n dim) n)))
          qs (vec (for [i (range q)]
                    (vec (subvec nums (+ (+ (* n dim) n 1) (* i dim)) (+ (+ (* n dim) n 1) (* (inc i) dim))))))
          nugsq (* nug nug)
          pairs (for [i (range n) j (range (inc i) n)] [i j])
          [num denom] (reduce (fn [[num denom] [i j]]
                                (let [rb (reduce + 0.0 (map (fn [k] (let [d (- (get-in pts [i k]) (get-in pts [j k]))] (* d d))) (range dim)))
                                      rb (Math/pow rb (* 0.5 beta))
                                      dv (- (nth vals i) (nth vals j))]
                                  [(+ num (* rb (- (* 0.5 (* dv dv)) nugsq)))
                                   (+ denom (* rb rb))]))
                              [0.0 0.0] pairs)
          alph (/ num denom)
          vgram (fn [r] (+ nugsq (* alph (Math/pow r beta))))
          m (inc n)
          aug (loop [i 0 v (vec (repeat m (vec (repeat m 0.0)))) y (vec (repeat m 0.0))]
                (if (>= i n)
                  [v y]
                  (let [v (loop [j i v v]
                            (if (>= j n)
                              v
                              (let [g (vgram (rdist (nth pts i) (nth pts j) dim))]
                                (recur (inc j) (assoc-in (assoc-in v [i j] g) [j i] g)))))]
                    (recur (inc i)
                           (assoc-in (assoc-in v [i n] 1.0) [n i] 1.0)
                           (assoc y i (nth vals i))))))
          v0 (nth aug 0)
          y0 (nth aug 1)
          [vlu p] (loop [k 0 vlu v0 p (vec (range m))]
                    (if (>= k m)
                      [vlu p]
                      (let [piv (loop [i (inc k) piv k]
                                  (if (>= i m)
                                    piv
                                    (if (> (absv (get-in vlu [i k])) (absv (get-in vlu [piv k])))
                                      (recur (inc i) i)
                                      (recur (inc i) piv))))
                            sw (fn [a x y] (let [t (nth a x)] (assoc (assoc a x (nth a y)) y t)))
                            vlu (if (not= piv k) (sw vlu k piv) vlu)
                            p (if (not= piv k) (sw p k piv) p)
                            vlu (reduce (fn [vlu i]
                                          (let [vlu (assoc-in vlu [i k] (/ (get-in vlu [i k]) (get-in vlu [k k])))]
                                            (reduce (fn [vlu j]
                                                      (assoc-in vlu [i j]
                                                                 (- (get-in vlu [i j])
                                                                    (* (get-in vlu [i k]) (get-in vlu [k j])))))
                                                    vlu (range (inc k) m))))
                                        vlu (range (inc k) m))]
                        (recur (inc k) vlu p))))
          yvi (lusolve vlu p y0)]
      (print (str/join "\n"
                       (map (fn [pt]
                              (let [vstar (loop [i 0 vs (vec (repeat m 0.0))]
                                            (if (>= i n)
                                              (assoc vs n 1.0)
                                              (recur (inc i) (assoc vs i (vgram (rdist pt (nth pts i) dim))))))
                                    lastval (reduce + 0.0 (map * yvi vstar))
                                    dstar (lusolve vlu p vstar)
                                    lasterr (reduce + 0.0 (map * dstar vstar))]
                                (str (fmt6 (canon lastval)) " " (fmt6 (canon (Math/sqrt (max 0.0 lasterr)))))))
                            qs))))))
