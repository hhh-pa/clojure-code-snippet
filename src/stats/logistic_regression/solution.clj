;; Logistic regression by IRLS (Think Stats 2e ch 11).
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

(defn- irls-matrix
  [rows beta p k]
  (reduce (fn [[mm vv] row]
            (let [cols (vec (cons 1.0 (map double (subvec row 0 p))))
                  lin (reduce + (map * cols beta))
                  pr (/ 1 (+ 1 (Math/exp (- lin))))
                  w (* pr (- 1 pr))]
              (if (== w 0.0)
                [mm vv]
                (let [z (+ lin (/ (- (double (nth row p)) pr) w))]
                  [(vec (map-indexed
                         (fn [i mrow]
                           (vec (map-indexed
                                 (fn [j cell] (+ cell (* (* w (nth cols i)) (nth cols j))))
                                 mrow)))
                         mm))
                   (vec (map-indexed
                         (fn [i cell] (+ cell (* (* w z) (nth cols i)))) vv))]))))
          [(vec (repeat k (vec (repeat k 0.0)))) (vec (repeat k 0.0))]
          rows))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          n (Long/parseLong (nth toks 0))
          p (Long/parseLong (nth toks 1))
          iters (Long/parseLong (nth toks 2))
          k (inc p)
          rows (mapv (fn [i] (mapv #(Long/parseLong (nth toks (+ 3 (* i k) %))) (range k)))
                     (range n))
          beta (loop [t 0 b (vec (repeat k 0.0))]
                 (if (>= t iters)
                   b
                   (let [[mat rhs] (irls-matrix rows b p k)
                         nb (gauss-solve mat rhs k)]
                     (if (nil? nb)
                       nil
                       (recur (inc t) nb)))))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (if (nil? beta)
        (print "SINGULAR")
        (let [good (count (filter true?
                                  (map (fn [row]
                                         (let [lin (reduce + (nth beta 0)
                                                             (map * (subvec beta 1)
                                                                  (map double (subvec row 0 p))))
                                               pr (/ 1 (+ 1 (Math/exp (- lin))))]
                                           (= (>= pr 0.5) (== (nth row p) 1))))
                                       rows)))]
          (print (str "BETA " (str/join " " (map fmt beta))
                      "\nACC " (fmt (/ (double good) n)))))))))
