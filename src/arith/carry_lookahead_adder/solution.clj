;; carry-lookahead adder: expanded g/p carry equations, then sum bits.
(require '[clojure.string :as str])

(defn add-bits [a b]
  (let [n (max (count a) (count b))
        pa (str (str/join (repeat (- n (count a)) \0)) a)
        pb (str (str/join (repeat (- n (count b)) \0)) b)
        g (mapv (fn [i] (bit-and (- (int (nth pa (- n 1 i))) (int \0))
                                   (- (int (nth pb (- n 1 i))) (int \0))))
                (range n))
        p (mapv (fn [i] (bit-xor (- (int (nth pa (- n 1 i))) (int \0))
                                   (- (int (nth pb (- n 1 i))) (int \0))))
                (range n))
        carries (reduce (fn [c i]
                          (let [carry (loop [acc (nth g i)
                                             prod 1
                                             j i]
                                        (if (< j 1)
                                          acc
                                          (let [prod (bit-and prod (nth p j))]
                                            (if (and (= prod 1) (= (nth g (dec j)) 1))
                                              1
                                              (recur acc prod (dec j))))))]
                            (assoc c (inc i) carry)))
                        (vec (repeat (inc n) 0))
                        (range n))]
    (apply str (nth carries n)
           (map (fn [i]
                  (let [x (- (int (nth pa (- n 1 i))) (int \0))
                        y (- (int (nth pb (- n 1 i))) (int \0))]
                    (bit-xor (bit-xor x y) (nth carries i))))
                (range (dec n) -1 -1)))))

(defn -main
  "Read two bit strings, print the (n+1)-bit lookahead sum."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 2)
      (print (add-bits (nth toks 0) (nth toks 1))))))

(-main (slurp *in*))
