;; Band-diagonal linear solver (NR 2.4): Bandec LU with partial pivoting
;; plus determinant. Port of the C++ reference (verbatim NR algorithm).
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
    (let [op (nth toks 0)
          n (Long/parseLong (nth toks 1))
          m1 (Long/parseLong (nth toks 2))
          m2 (Long/parseLong (nth toks 3))
          nums (mapv #(Double/parseDouble %) (subvec toks 4))
          full (vec (map #(vec (subvec nums (* % n) (+ (* % n) n))) (range n)))
          b (if (= op "SOLVE") (vec (subvec nums (* n n) (+ (* n n) n))) (vec (repeat n 0.0)))
          mm (+ m1 m2 1)
          au0 (vec (for [i (range n)]
                     (let [row (vec (repeat mm 0.0))]
                       (reduce (fn [r k] (assoc r (+ (- k i) m1) (nth (nth full i) k)))
                               row (range (max 0 (- i m1)) (inc (min (dec n) (+ i m2))))))))
          al (vec (repeat n (vec (repeat (max m1 1) 0.0))))
          tiny 1.0e-40
          ;; rearrange (verbatim NR)
          au (loop [au au0 i 0 l m1]
               (if (>= i m1) au
                   (let [au (loop [au au j (- m1 i)]
                              (if (>= j mm) au
                                  (recur (assoc-in au [i (- j l)] (get-in au [i j])) (inc j))))
                         l (dec l)
                         au (loop [au au j (- mm l 1)]
                              (if (>= j mm) au
                                  (recur (assoc-in au [i j] 0.0) (inc j))))]
                     (recur au (inc i) l))))
          ;; decompose (verbatim NR)
          [au al indx dd] (loop [k 0 au au al al indx (vec (repeat n 0)) dd 1.0 l m1]
                            (if (>= k n) [au al indx dd]
                                (let [l (if (< l n) (inc l) l)
                                      [dum ii] (loop [j (inc k) dum (get-in au [k 0]) ii k]
                                                 (if (>= j l) [dum ii]
                                                     (if (> (absv (get-in au [j 0])) (absv dum))
                                                       (recur (inc j) (get-in au [j 0]) j)
                                                       (recur (inc j) dum ii))))
                                      indx (assoc indx k (inc ii))
                                      au (if (zero? dum) (assoc-in au [k 0] tiny) au)
                                      dd (if (not= ii k) (- dd) dd)
                                      au (if (not= ii k)
                                           (loop [au au j 0]
                                             (if (>= j mm) au
                                                 (let [t (get-in au [k j])]
                                                   (recur (assoc-in (assoc-in au [k j] (get-in au [ii j])) [ii j] t) (inc j)))))
                                           au)
                                      [au al] (loop [i2 (inc k) au au al al]
                                                (if (>= i2 l) [au al]
                                                    (let [d2 (/ (get-in au [i2 0]) (get-in au [k 0]))
                                                          al (assoc-in al [k (- i2 k 1)] d2)
                                                          au (loop [au au j 1]
                                                                 (if (>= j mm) au
                                                                     (recur (assoc-in au [i2 (dec j)]
                                                                                      (- (get-in au [i2 j]) (* d2 (get-in au [k j]))))
                                                                            (inc j))))]
                                                      (recur (inc i2) (assoc-in au [i2 (dec mm)] 0.0) al))))]
                                  (recur (inc k) au al indx dd l))))]
      (if (= op "DET")
        (print (fmt6 (canon (reduce * dd (map #(nth % 0) au)))))
        (let [x (loop [k 0 x (vec b) l m1]
                  (if (>= k n) [x l]
                      (let [j (dec (nth indx k))
                            x (if (not= j k)
                                (let [t (nth x k)] (assoc (assoc x k (nth x j)) j t))
                                x)
                            l (if (< l n) (inc l) l)
                            x (loop [x x j (inc k)]
                                (if (>= j l) x
                                    (recur (assoc x j (- (nth x j) (* (get-in al [k (- j k 1)]) (nth x k)))) (inc j))))]
                        (recur (inc k) x l))))
              x (first x)
              x (loop [i (dec n) x x l 1]
                  (if (< i 0) x
                      (let [dum (loop [k 1 d (nth x i)]
                                  (if (>= k l) d
                                      (recur (inc k) (- d (* (get-in au [i k]) (nth x (+ k i)))))))]
                        (recur (dec i) (assoc x i (/ dum (get-in au [i 0])))
                               (if (< l mm) (inc l) l)))))]
          (print (str/join " " (map #(fmt6 (canon %)) x))))))))
