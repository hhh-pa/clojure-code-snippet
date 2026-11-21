;; Rational Chebyshev approximation (NR 5.13, ratlsq). Port of the C++ reference.
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

(defn feval [fun p t]
  (cond (= fun "POLY") (reduce (fn [s c] (+ (* s t) c)) 0.0 (reverse p))
        (= fun "EXP") (+ (* (nth p 0) (Math/exp (* (nth p 1) t))) (nth p 2))
        (= fun "TRIG") (* (nth p 0) (Math/sin (+ (* (nth p 1) t) (nth p 2))))
        :else (* (nth p 0) (Math/log (+ (* (nth p 1) t) (nth p 2))))))

(defn jacobi-pair [bb v m p q]
  (let [alpha (reduce (fn [s k] (+ s (* (get-in bb [k p]) (get-in bb [k p])))) 0.0 (range m))
        gamma (reduce (fn [s k] (+ s (* (get-in bb [k q]) (get-in bb [k q])))) 0.0 (range m))
        beta (reduce (fn [s k] (+ s (* (get-in bb [k p]) (get-in bb [k q])))) 0.0 (range m))]
    (if (zero? beta) [bb v]
        (let [zeta (/ (- gamma alpha) (* 2.0 beta))
              t (/ (if (neg? zeta) -1.0 1.0) (+ (absv zeta) (Math/sqrt (+ 1.0 (* zeta zeta)))))
              c (/ 1.0 (Math/sqrt (+ 1.0 (* t t))))
              s (* c t)
              bb (reduce (fn [bb k]
                           (let [bp (get-in bb [k p]) bq (get-in bb [k q])]
                             (-> bb (assoc-in [k p] (- (* c bp) (* s bq)))
                                (assoc-in [k q] (+ (* s bp) (* c bq))))))
                         bb (range m))
              n (count v)
              v (reduce (fn [v k]
                          (let [vp (get-in v [k p]) vq (get-in v [k q])]
                            (-> v (assoc-in [k p] (- (* c vp) (* s vq)))
                               (assoc-in [k q] (+ (* s vp) (* c vq))))))
                        v (range n))]
          [bb v]))))

(defn jacobi-sweep [bb v m n]
  (loop [p 0 bb bb v v]
    (if (>= p (dec n)) [bb v]
        (let [[bb v] (loop [q (inc p) bb bb v v]
                       (if (>= q n) [bb v]
                           (let [[bb v] (jacobi-pair bb v m p q)]
                             (recur (inc q) bb v))))]
          (recur (inc p) bb v)))))

(defn svdsolve [a b]
  (let [m (count a)
        n (count (nth a 0))
        [bb v] (loop [sw 0 bb a v (vec (for [i (range n)] (vec (for [j (range n)] (if (= i j) 1.0 0.0)))))]
                 (if (>= sw 12) [bb v]
                     (let [[bb v] (jacobi-sweep bb v m n)]
                       (recur (inc sw) bb v))))
        w (vec (for [j (range n)]
                 (Math/sqrt (reduce (fn [s k] (+ s (* (get-in bb [k j]) (get-in bb [k j])))) 0.0 (range m)))))
        u (vec (for [i (range m)]
                 (vec (for [j (range n)]
                        (if (zero? (nth w j)) 0.0 (/ (get-in bb [i j]) (nth w j)))))))
        wmax (reduce max 0.0 w)
        thresh (* 1.0e-13 wmax)
        tmp (vec (for [j (range n)]
                   (if (> (nth w j) thresh)
                     (/ (reduce (fn [s i] (+ s (* (get-in u [i j]) (nth b i)))) 0.0 (range m)) (nth w j))
                     0.0)))]
    (vec (for [j (range n)]
           (reduce (fn [s jj] (+ s (* (get-in v [j jj]) (nth tmp jj)))) 0.0 (range n))))))

(defn rat-eval [coff mm kk x]
  (let [num (reduce (fn [s t] (+ (* s x) (nth coff t))) 0.0 (range mm -1 -1))
        den (+ (reduce (fn [s t] (+ (* s x) (nth coff (+ mm t)))) 0.0 (range kk 0 -1)) 0.0)]
    (/ num (+ (* den x) 1.0))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [fun (nth toks 0)
          [p rest] (if (= fun "POLY")
                     (let [d (Long/parseLong (nth toks 1))]
                       [(vec (for [i (range (inc d))] (Double/parseDouble (nth toks (+ 2 i)))))
                        (+ 3 d)])
                     [(vec (for [i (range 3)] (Double/parseDouble (nth toks (+ 1 i))))) 4])
          a (Double/parseDouble (nth toks rest))
          b (Double/parseDouble (nth toks (inc rest)))
          mm (Long/parseLong (nth toks (+ rest 2)))
          kk (Long/parseLong (nth toks (+ rest 3)))
          pio2 (/ Math/PI 2.0)
          ncof (+ mm kk 1)
          npt (* 8 ncof)
          xs (vec (for [i (range npt)]
                    (if (< i (- (quot npt 2) 1))
                      (let [hth (/ (* pio2 i) (- npt 1.0)) sn (Math/sin hth)]
                        (+ a (* (- b a) sn sn)))
                      (let [hth (/ (* pio2 (- npt i)) (- npt 1.0)) sn (Math/sin hth)]
                        (- b (* (- b a) sn sn))))))
          fs (mapv #(feval fun p %) xs)
          [best dev] (loop [it 0 wt (vec (repeat npt 1.0)) ee (vec (repeat npt 1.0))
                            e 0.0 best (vec (repeat ncof 0.0)) dev 1.0e99]
                       (if (>= it 5) [best dev]
                           (let [bbv (vec (for [i (range npt)]
                                            (let [se (if (neg? (nth ee i)) (- (absv e)) (absv e))]
                                              (* (nth wt i) (+ (nth fs i) se)))))
                                 u (vec (for [i (range npt)]
                                          (let [rowp (loop [j 0 power (nth wt i) row []]
                                                       (if (> j mm) [row power]
                                                           (recur (inc j) (* power (nth xs i)) (conj row power))))
                                                [row _] rowp
                                                rowq (loop [j (inc mm) power (- (nth bbv i)) row row]
                                                       (if (>= j ncof) row
                                                           (let [power (* power (nth xs i))]
                                                             (recur (inc j) power (conj row power)))))]
                                            (vec rowq))))
                                 coff (svdsolve u bbv)
                                 devs (mapv (fn [j] (- (rat-eval coff mm kk (nth xs j)) (nth fs j))) (range npt))
                                 wt (mapv absv devs)
                                 devmax (reduce max 0.0 wt)
                                 e (/ (reduce + 0.0 wt) npt)]
                             (if (<= devmax dev)
                               (recur (inc it) wt devs e coff devmax)
                               (recur (inc it) wt devs e best dev)))))]
      (println (str/join " " (map fmtv (subvec best 0 (inc mm)))))
      (println (str/join " " (map fmtv (subvec best (inc mm)))))
      (println (fmt6 (+ dev 0.0))))))
