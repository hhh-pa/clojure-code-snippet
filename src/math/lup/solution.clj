;; LUP decomposition + solve with partial pivoting (CLRS 28).
(require '[clojure.string :as str])
(import '[java.util Locale])

(defn- lup-decompose
  [a perm]
  (let [n (count a)]
    (loop [k 0 a a perm perm]
      (if (>= k n)
        [a perm]
        (let [mag (fn [i] (Math/abs (double (nth (nth a i) k))))
              piv (reduce (fn [p i] (if (> (mag i) (mag p)) i p)) k (range (inc k) n))]
          (if (zero? (nth (nth a piv) k))
            nil
            (let [[a perm] (if (= piv k)
                                 [a perm]
                                 [(assoc a k (nth a piv) piv (nth a k))
                                  (assoc perm k (nth perm piv) piv (nth perm k))])
                  a (loop [i (inc k) a a]
                      (if (>= i n)
                        a
                        (let [f (/ (nth (nth a i) k) (nth (nth a k) k))]
                          (recur (inc i)
                                 (loop [j (inc k) r (assoc-in a [i k] f)]
                                   (if (>= j n)
                                     r
                                     (recur (inc j)
                                            (assoc-in r [i j]
                                                      (- (nth (nth r i) j)
                                                         (* f (nth (nth r k) j)))))))))))]
              (recur (inc k) a perm))))))))

(defn- lup-solve
  [a perm b]
  (let [n (count a)
        y (loop [i 0 y (vec (repeat n 0.0))]
            (if (>= i n)
              y
              (recur (inc i)
                     (assoc y i (loop [j 0 s (nth b (nth perm i))]
                                      (if (>= j i)
                                        s
                                        (recur (inc j)
                                               (- s (* (nth (nth a i) j) (nth y j))))))))))
        x (loop [i (dec n) x (vec (repeat n 0.0))]
            (if (neg? i)
              x
              (recur (dec i)
                     (assoc x i (/ (loop [j (inc i) s (nth y i)]
                                         (if (>= j n)
                                           s
                                           (recur (inc j)
                                                  (- s (* (nth (nth a i) j) (nth x j))))))
                                   (nth (nth a i) i))))))]
    x))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (str/split (str/trim s) #"\s+")
          n (Long/parseLong (first toks))
          nums (mapv #(Double/parseDouble %) (rest toks))
          a (mapv vec (partition n (take (* n n) nums)))
          b (vec (take n (drop (* n n) nums)))
          perm (vec (range n))
          fmt (fn [v] (String/format Locale/US "%.6f"
                                     (to-array [(if (< (Math/abs v) 5e-7) 0.0 v)])))
          res (lup-decompose a perm)]
      (if (nil? res)
        (print "SINGULAR")
        (let [[lu pf] res
              x (lup-solve lu pf b)]
          (print (str/join " " (map fmt x))))))))
