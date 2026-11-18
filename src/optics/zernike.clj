;; Zernike fit + RMS (geom ch 13).
(require '[clojure.string :as str])
(import '[java.util Locale])

(defn- snap [x] (if (< (Math/abs x) 5e-7) 0.0 x))

(defn- fact [n] (reduce * 1 (range 2 (inc n))))

(defn- nm-of
  [j]
  (let [n (loop [nn 0]
            (if (< (quot (* (inc nn) (+ nn 2)) 2) j)
              (recur (inc nn))
              nn))
        r (- j (quot (* n (inc n)) 2) 1)
        m (if (even? n)
            (cond (== r 0) 0
                  (odd? r) (inc r)
                  :else (- r))
            (if (even? r) (inc r) (- r)))]
    [n m]))

(defn- radial
  [n m ro]
  (let [am (Math/abs m)]
    (reduce + (map (fn [s]
                     (let [num (fact (- n s))
                           den (* (fact s)
                                  (fact (- (quot (+ n am) 2) s))
                                  (fact (- (quot (- n am) 2) s)))
                           pw (reduce * 1.0 (repeat (- n (* 2 s)) ro))
                           term (* (/ (double num) den) pw)]
                       (if (even? s) term (- term))))
                   (range (inc (quot (- n am) 2)))))))

(defn- zernike
  [j ro th]
  (let [[n m] (nm-of j)
        r (radial n m ro)]
    (cond (== m 0) (* (Math/sqrt (+ n 1)) r)
          (> m 0) (* (Math/sqrt (* 2 (+ n 1))) r (Math/cos (* m th)))
          :else (* (Math/sqrt (* 2 (+ n 1))) r (Math/sin (* (- m) th))))))

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
          jn (Long/parseLong (nth toks 0))
          n (Long/parseLong (nth toks 1))
          ros (mapv #(Double/parseDouble (nth toks (+ 2 (* 3 %)))) (range n))
          ths (mapv #(Double/parseDouble (nth toks (+ 3 (* 3 %)))) (range n))
          ws (mapv #(Double/parseDouble (nth toks (+ 4 (* 3 %)))) (range n))
          cols (mapv (fn [i] (mapv #(zernike % (nth ros i) (nth ths i)) (range 1 (inc jn))))
                     (range n))
          [mat rhs] (reduce (fn [[mm vv] i]
                              (let [row (nth cols i)]
                                [(vec (map-indexed
                                       (fn [a mrow]
                                         (vec (map-indexed
                                               (fn [b cell] (+ cell (* (nth row a) (nth row b))))
                                               mrow)))
                                       mm))
                                 (vec (map-indexed
                                       (fn [a cell] (+ cell (* (nth row a) (nth ws i)))) vv))]))
                            [(vec (repeat jn (vec (repeat jn 0.0)))) (vec (repeat jn 0.0))]
                            (range n))
          beta (gauss-solve mat rhs jn)
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (if (nil? beta)
        (print "SINGULAR")
        (let [[ssres ssdat]
              (reduce (fn [[rr tt] i]
                        (let [pred (reduce + (map * beta (nth cols i)))
                              r (- (nth ws i) pred)]
                          [(+ rr (* r r)) (+ tt (* (nth ws i) (nth ws i)))]))
                      [0.0 0.0] (range n))
              rms (Math/sqrt (/ ssres n))
              rmsd (Math/sqrt (/ ssdat n))]
          (print (str "COEFFS " (str/join " " (map (comp fmt snap) beta))
                      "\nRMS " (fmt (snap rms))
                      "\nRMSDATA " (fmt (snap rmsd)))))))))
