;; Pade approximants (NR 5.12).
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

(defn lusolve [lu p rhs n]
  (let [y (reduce (fn [y i]
                    (assoc y i (reduce (fn [s j] (- s (* (get-in lu [i j]) (nth y j))))
                                       (nth rhs (nth p i)) (range i))))
                  (vec (repeat n 0.0)) (range n))]
    (reduce (fn [x i]
              (assoc x i (/ (reduce (fn [s j] (- s (* (get-in lu [i j]) (nth x j))))
                                     (nth y i) (range (inc i) n))
                            (get-in lu [i i]))))
            (vec (repeat n 0.0)) (range (dec n) -1 -1))))

(defn lufactor [a0 n]
  (loop [k 0 lu a0 p (vec (range n))]
    (if (>= k n) [lu p]
        (let [piv (loop [i (inc k) piv k]
                    (if (>= i n) piv
                        (recur (inc i) (if (> (absv (get-in lu [i k])) (absv (get-in lu [piv k]))) i piv))))
              lu (if (= piv k) lu (assoc lu k (nth lu piv) piv (nth lu k)))
              p (if (= piv k) p (assoc p k (nth p piv) piv (nth p k)))
              lu (reduce (fn [lu i]
                           (let [lu (assoc-in lu [i k] (/ (get-in lu [i k]) (get-in lu [k k])))]
                             (reduce (fn [lu j]
                                       (assoc-in lu [i j] (- (get-in lu [i j])
                                                              (* (get-in lu [i k]) (get-in lu [k j])))))
                                     lu (range (inc k) n))))
                         lu (range (inc k) n))]
          (recur (inc k) lu p)))))

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [n (Long/parseLong (nth toks 0))
          c (vec (for [i (range (inc (* 2 n)))] (Double/parseDouble (nth toks (+ 1 i)))))
          a0 (vec (for [k (range 1 (inc n))]
                    (vec (for [m (range 1 (inc n))] (nth c (+ (- n m) k))))))
          rhs (vec (for [k (range 1 (inc n))] (- (nth c (+ n k)))))
          [lu p] (lufactor a0 n)
          bb (lusolve lu p rhs n)
          r (vec (for [i (range n)]
                   (reduce (fn [s j] (+ s (* (get-in a0 [i j]) (nth bb j))))
                           (- (nth rhs i)) (range n))))
          dx (lusolve lu p r n)
          bb (vec (for [i (range n)] (- (nth bb i) (nth dx i))))
          b (vec (for [m (range (inc n))] (if (zero? m) 1.0 (nth bb (dec m)))))
          av (vec (for [k (range (inc n))]
                    (if (zero? k) (nth c 0)
                        (reduce (fn [s m] (+ s (* (nth b m) (nth c (- k m))))) 0.0 (range (inc k))))))]
      (println (str/join " " (map fmtv av)))
      (println (str/join " " (map fmtv (subvec b 1)))))))
