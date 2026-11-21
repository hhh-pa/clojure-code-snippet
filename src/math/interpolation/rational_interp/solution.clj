;; Rational interpolation (NR 3.4). Port of the C++ reference.
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

(defn ratint [xa ya x]
  (let [n (count xa)
        tiny 1.0e-99
        [ns exact] (loop [i 0 ns 0 hh (absv (- x (nth xa 0))) exact nil]
                     (if (>= i n) [ns exact]
                         (let [h (absv (- x (nth xa i)))]
                           (cond (zero? h) (recur (inc i) ns hh i)
                                 (and (< h hh) (nil? exact)) (recur (inc i) i h exact)
                                 :else (recur (inc i) ns hh exact)))))]
    (if (some? exact)
      [(nth ya exact) 0.0]
      (loop [m 1 y (nth ya ns) dy 0.0
             c (vec ya) d (mapv + ya (repeat tiny)) ns (dec ns)]
        (if (>= m n) [y dy]
            (let [[c d] (loop [i 0 c c d d]
                          (if (>= i (- n m)) [c d]
                              (let [w (- (nth c (inc i)) (nth d i))
                                    h (- (nth xa (+ i m)) x)
                                    t (/ (* (- (nth xa i) x) (nth d i)) h)
                                    dd (/ w (- t (nth c (inc i))))]
                                (recur (inc i)
                                       (assoc c i (* t dd))
                                       (assoc d i (* (nth c (inc i)) dd))))))
                  [dy2 ns2] (if (< (* 2 (inc ns)) (- n m))
                              [(nth c (inc ns)) ns]
                              [(nth d ns) (dec ns)])]
              (recur (inc m) (+ y dy2) dy2 c d ns2)))))))

(defn baryrat [xa ya ddord x]
  (let [n (count xa)]
    (if (some #(zero? (- x (nth xa %))) (range n))
      (nth ya (first (filter #(zero? (- x (nth xa %))) (range n))))
      (let [w (mapv (fn [k]
                      (let [imin (max (- k ddord) 0)
                            imax (if (>= k (- n ddord)) (- (- n ddord) 1) k)]
                        (loop [i imin temp (if (odd? imin) -1.0 1.0) sum 0.0]
                          (if (> i imax) sum
                              (let [term (loop [j i term 1.0]
                                           (if (> j (min (+ i ddord) (dec n))) term
                                               (recur (inc j) (if (= j k) term (* term (- (nth xa k) (nth xa j)))))))]
                                (recur (inc i) (- temp) (+ sum (/ temp term))))))))
                    (range n))
            [num den] (reduce (fn [[num den] i]
                                (let [t (/ (nth w i) (- x (nth xa i)))]
                                  [(+ num (* t (nth ya i))) (+ den t)]))
                              [0.0 0.0] (range n))]
        (/ num den)))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [op (nth toks 0)
          n (Long/parseLong (nth toks 1))
          nums (mapv #(Double/parseDouble %) (subvec toks 2))
          xa (vec (subvec nums 0 n))
          ya (vec (subvec nums n (+ n n)))
          restv (subvec nums (+ n n))
          [dd _q qs] (if (= op "BARYRAT")
                      [(long (nth restv 0)) (long (nth restv 1)) (subvec restv 2 (+ 2 (long (nth restv 1))))]
                      [0 (long (nth restv 0)) (subvec restv 1 (+ 1 (long (nth restv 0))))])]
      (print (str/join "\n"
                       (if (= op "RATINT")
                         (map (fn [x] (let [[y dy] (ratint xa ya x)]
                                        (str (fmt6 (canon y)) " " (fmt6 (canon dy))))) qs)
                         (map #(fmt6 (canon (baryrat xa ya dd %))) qs)))))))
