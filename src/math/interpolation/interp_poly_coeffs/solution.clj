;; Coefficients of the interpolating polynomial (NR 3.5, polcof).
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

(defn neville [xa ya m x]
  (let [[ns _] (loop [i 0 ns 0 dif (absv (- x (nth xa 0)))]
                 (if (>= i m) [ns dif]
                     (let [dift (absv (- x (nth xa i)))]
                       (if (< dift dif)
                         (recur (inc i) i dift)
                         (recur (inc i) ns dif)))))
        [y _ _ _] (loop [mm 1 y (nth ya ns) c (vec (subvec (vec ya) 0 m))
                         d (vec (subvec (vec ya) 0 m)) ns (dec ns)]
                    (if (>= mm m) [y c d ns]
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

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [n (Long/parseLong (nth toks 0))
          nums (mapv #(Double/parseDouble %) (subvec toks 1))
          x (vec (subvec nums 0 n))
          y (vec (subvec nums n (+ n n)))
          cof (loop [j 0 x x y y cof []]
                (if (>= j n) cof
                    (let [m (- n j)
                          cj (neville x y m 0.0)
                          [k _] (loop [i 0 k -1 xmin 1.0e99]
                                  (if (>= i m) [k xmin]
                                      (recur (inc i)
                                             (if (< (absv (nth x i)) xmin) i k)
                                             (min xmin (absv (nth x i))))))
                          y (mapv (fn [i] (if (zero? (nth x i)) (nth y i)
                                              (/ (- (nth y i) cj) (nth x i)))) (range m))
                          x (vec (concat (subvec x 0 k) (subvec x (inc k) m)))
                          y (vec (concat (subvec y 0 k) (subvec y (inc k) m)))]
                      (recur (inc j) x y (conj cof cj)))))]
      (print (str/join " " (map #(fmt6 (canon %)) cof))))))
