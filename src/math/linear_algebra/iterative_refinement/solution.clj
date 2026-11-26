;; Iterative refinement of a linear solve (NR 2.5, mprove).
;;
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

(defn lusolve [lu p rhs n]
  (let [y (loop [i 0 y (vec (repeat n 0.0))]
            (if (>= i n) y
                (recur (inc i) (assoc y i (loop [j 0 s (nth rhs (nth p i))]
                                                  (if (>= j i) s
                                                      (recur (inc j) (- s (* (get-in lu [i j]) (nth y j))))))))))
        x (loop [i (dec n) x (vec (repeat n 0.0))]
            (if (< i 0) x
                (let [s (loop [j (inc i) s (nth y i)]
                            (if (>= j n) s
                                (recur (inc j) (- s (* (get-in lu [i j]) (nth x j))))))]
                  (recur (dec i) (assoc x i (/ s (get-in lu [i i])))))))]
    x))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [n (Long/parseLong (nth toks 0))
          nums (mapv #(Double/parseDouble %) (subvec toks 1))
          a (vec (map #(vec (subvec nums (* % n) (+ (* % n) n))) (range n)))
          b (vec (subvec nums (* n n) (+ (* n n) n)))
          [lu p] (loop [k 0 lu a p (vec (range n))]
                   (if (>= k n) [lu p]
                       (let [piv (loop [i (inc k) piv k]
                                   (if (>= i n) piv
                                       (if (> (absv (get-in lu [i k])) (absv (get-in lu [piv k])))
                                         (recur (inc i) i)
                                         (recur (inc i) piv))))
                             lu (if (not= piv k)
                                  (let [t (nth lu k)] (assoc (assoc lu k (nth lu piv)) piv t))
                                  lu)
                             p (if (not= piv k)
                                 (let [t (nth p k)] (assoc (assoc p k (nth p piv)) piv t))
                                 p)
                             lu (loop [i (inc k) lu lu]
                                  (if (>= i n) lu
                                      (let [lu (assoc-in lu [i k] (/ (get-in lu [i k]) (get-in lu [k k])))]
                                        (recur (inc i) (loop [j (inc k) lu lu]
                                                          (if (>= j n) lu
                                                              (recur (inc j) (assoc-in lu [i j]
                                                                                       (- (get-in lu [i j])
                                                                                          (* (get-in lu [i k]) (get-in lu [k j])))))))))))]
                         (recur (inc k) lu p))))
          x (lusolve lu p b n)
          x (loop [round 0 x x]
              (if (>= round 2) x
                  (let [r (vec (for [i (range n)]
                                  (loop [j 0 s (- (nth b i)) c 0.0]
                                    (if (>= j n) s
                                        (let [y (- (* (get-in a [i j]) (nth x j)) c)
                                              t (+ s y)]
                                          (recur (inc j) t (- (- t s) y)))))))
                        dx (lusolve lu p r n)]
                    (recur (inc round) (mapv - x dx)))))]
      (print (str/join " " (map #(fmt6 (canon %)) x))))))
