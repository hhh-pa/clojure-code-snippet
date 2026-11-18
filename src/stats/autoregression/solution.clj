;; Autoregression AR(p) fit + forecast (Think Stats 2e ch 12).
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
          f (Long/parseLong (nth toks 2))
          xs (mapv #(Long/parseLong %) (subvec toks 3 (+ 3 n)))
          k (inc p)
          [mat rhs] (reduce (fn [[mm vv] t]
                              (let [cols (vec (cons 1.0 (map #(double (nth xs (- t %)))
                                                            (range 1 k))))
                                    y (double (nth xs t))]
                                [(vec (map-indexed
                                       (fn [i mrow]
                                         (vec (map-indexed
                                               (fn [j cell] (+ cell (* (nth cols i) (nth cols j))))
                                               mrow)))
                                       mm))
                                 (vec (map-indexed
                                       (fn [i cell] (+ cell (* (nth cols i) y))) vv))]))
                            [(vec (repeat k (vec (repeat k 0.0)))) (vec (repeat k 0.0))]
                            (range p n))
          beta (gauss-solve mat rhs k)
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (if (nil? beta)
        (print "SINGULAR")
        (let [[_ outs] (reduce (fn [[hist os] _]
                                 (let [pred (reduce + (nth beta 0)
                                                      (map-indexed
                                                       (fn [jj b]
                                                         (* b (nth hist (- (count hist) (inc jj)))))
                                                       (subvec beta 1)))]
                                   [(conj hist pred) (conj os (fmt pred))]))
                               [(mapv double xs) []] (range f))]
          (print (str "BETA " (str/join " " (map fmt beta))
                      "\nFORECAST " (str/join " " outs))))))))
