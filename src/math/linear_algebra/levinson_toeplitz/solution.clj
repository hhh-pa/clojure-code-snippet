;; Toeplitz solve in O(n^2) (NR 2.8.2, toeplz). Port of the C++ reference.
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

(let [toks (vec (str/split (str/trim (slurp *in*)) #"\s+"))]
  (when (seq toks)
    (let [n (Long/parseLong (nth toks 0))
          nums (mapv #(Double/parseDouble %) (subvec toks 1))
          r (vec (subvec nums 0 (dec (* 2 n))))
          y (vec (subvec nums (dec (* 2 n)) (+ (dec (* 2 n)) n)))
          n1 (dec n)
          x (loop [m 0
                   x (assoc (vec (repeat n 0.0)) 0 (/ (nth y 0) (nth r n1)))
                   g (assoc (vec (repeat (max n1 1) 0.0)) 0 (if (zero? n1) 0.0 (/ (nth r (dec n1)) (nth r n1))))
                   h (assoc (vec (repeat (max n1 1) 0.0)) 0 (if (zero? n1) 0.0 (/ (nth r (inc n1)) (nth r n1))))]
              (if (or (zero? n1) (> m (dec n1)))
                x
                (let [m1 (inc m)
                      [sxn sd] (loop [j 0 sxn (- (nth y m1)) sd (- (nth r n1))]
                                 (if (> j m) [sxn sd]
                                     (recur (inc j)
                                            (+ sxn (* (nth r (+ n1 m1 (- j))) (nth x j)))
                                            (+ sd (* (nth r (+ n1 m1 (- j))) (nth g (- m j)))))))
                      x (assoc x m1 (/ sxn sd))
                      x (loop [j 0 x x]
                          (if (> j m) x
                              (recur (inc j) (assoc x j (- (nth x j) (* (nth x m1) (nth g (- m j))))))))]
                  (if (= m1 n1)
                    x
                    (let [[sgn shn sgd] (loop [j 0 sgn (- (nth r (- n1 m1 1))) shn (- (nth r (+ n1 m1 1))) sgd (- (nth r n1))]
                                           (if (> j m) [sgn shn sgd]
                                               (recur (inc j)
                                                      (+ sgn (* (nth r (+ n1 j (- m1))) (nth g j)))
                                                      (+ shn (* (nth r (+ n1 m1 (- j))) (nth h j)))
                                                      (+ sgd (* (nth r (+ n1 j (- m1))) (nth h (- m j)))))))
                          g (assoc g m1 (/ sgn sgd))
                          h (assoc h m1 (/ shn sd))
                          pp (nth g m1) qq (nth h m1)
                          m2 (quot (+ m 2) 2)
                          [g h] (loop [j 0 k m g g h h]
                                  (if (>= j m2) [g h]
                                      (let [pt1 (nth g j) pt2 (nth g k)
                                            qt1 (nth h j) qt2 (nth h k)]
                                        (recur (inc j) (dec k)
                                               (assoc (assoc g j (- pt1 (* pp qt2))) k (- pt2 (* pp qt1)))
                                               (assoc (assoc h j (- qt1 (* qq pt2))) k (- qt2 (* qq pt1)))))))]
                      (recur (inc m) x g h))))))]
      (print (str/join " " (map #(fmt6 (canon %)) x))))))
