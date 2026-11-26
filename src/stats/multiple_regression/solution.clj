;; OLS multiple regression (Think Stats 2e ch 11).
(require '[clojure.string :as str])
(import '[java.util Locale])

(defn- gauss-solve
  [mat rhs k]
  (loop [s 0 m mat v rhs]
    (if (>= s k)
      (loop [s (dec k) beta (vec (repeat k 0.0))]
        (if (< s 0)
          beta
          (let [tot (reduce (fn [t c] (- t (* (nth (nth m s) c) (nth beta c))))
                            (nth v s) (range (inc s) k))]
            (recur (dec s) (assoc beta s (/ tot (nth (nth m s) s)))))))
      (let [piv (reduce (fn [best r]
                          (if (> (Math/abs (nth (nth m r) s))
                                 (Math/abs (nth (nth m best) s)))
                            r
                            best))
                        s (range (inc s) k))
            m2 (assoc m s (nth m piv) piv (nth m s))
            v2 (assoc v s (nth v piv) piv (nth v s))]
        (if (== (nth (nth m2 s) s) 0.0)
          nil
          (let [[m3 v3] (reduce (fn [[mm vv] r]
                                  (let [factor (/ (nth (nth mm r) s) (nth (nth mm s) s))
                                        row (reduce (fn [rw c]
                                                      (assoc rw c (- (nth rw c)
                                                                      (* factor (nth (nth mm s) c)))))
                                                    (nth mm r) (range s k))]
                                    [(assoc mm r row)
                                     (assoc vv r (- (nth vv r) (* factor (nth vv s))))]))
                                [m2 v2] (range (inc s) k))]
            (recur (inc s) m3 v3)))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          n (Long/parseLong (nth toks 0))
          p (Long/parseLong (nth toks 1))
          k (inc p)
          rows (mapv (fn [i] (mapv #(Long/parseLong (nth toks (+ 2 (* i k) %))) (range k)))
                     (range n))
          cols-of (fn [row] (vec (cons 1.0 (map double (subvec row 0 p)))))
          [mat rhs] (reduce (fn [[mm vv] row]
                              (let [cols (cols-of row)
                                    y (double (nth row p))]
                                [(vec (map-indexed
                                       (fn [i mrow]
                                         (vec (map-indexed
                                               (fn [j cell] (+ cell (* (nth cols i) (nth cols j))))
                                               mrow)))
                                       mm))
                                 (vec (map-indexed
                                       (fn [i cell] (+ cell (* (nth cols i) y))) vv))]))
                            [(vec (repeat k (vec (repeat k 0.0)))) (vec (repeat k 0.0))]
                            rows)
          beta (gauss-solve mat rhs k)
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (if (nil? beta)
        (print "SINGULAR")
        (let [my (/ (reduce + (map #(nth % p) rows)) n)
              [ssres sstot]
              (reduce (fn [[rr tt] row]
                        (let [pred (reduce (fn [acc j] (+ acc (* (nth beta j)
                                                                 (double (nth row (dec j))))))
                                           (nth beta 0) (range 1 k))
                              r (- (double (nth row p)) pred)
                              d (- (double (nth row p)) my)]
                          [(+ rr (* r r)) (+ tt (* d d))]))
                      [0.0 0.0] rows)
              r2 (if (== sstot 0.0) 0.0 (- 1 (/ ssres sstot)))]
          (print (str "BETA " (str/join " " (map fmt beta)) "\nRSQ " (fmt r2))))))))
