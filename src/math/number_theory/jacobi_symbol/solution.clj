;; Jacobi symbol via the quadratic-reciprocity loop.
(require '[clojure.string :as str])

(defn jacobi [a0 n0]
  (loop [a (rem a0 n0)
         n n0
         t 1]
    (if (zero? a)
      (if (= n 1) t 0)
      (let [[a t] (loop [a a
                         t t]
                    (if (pos? (rem a 2))
                      [a t]
                      (let [r (rem n 8)]
                        (recur (quot a 2) (if (or (= r 3) (= r 5)) (- t) t)))))]
        (recur (rem n a) a (if (and (= (rem a 4) 3) (= (rem n 4) 3)) (- t) t))))))

(defn -main
  "Read a and n; print the Jacobi symbol -1, 0 or 1."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 2)
      (print (jacobi (Long/parseLong (nth toks 0)) (Long/parseLong (nth toks 1)))))))

(-main (slurp *in*))
