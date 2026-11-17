;; carry-select adder: 4-bit blocks ripple twice (cin 0/1), mux by carry-in.
(require '[clojure.string :as str])

(defn add-bits [a b]
  (let [n (max (count a) (count b))
        pa (str (str/join (repeat (- n (count a)) \0)) a)
        pb (str (str/join (repeat (- n (count b)) \0)) b)
        k 4]
    (loop [base 0
           carry 0
           out (vec (repeat (inc n) \0))]
      (if (>= base n)
        (apply str (assoc out 0 (char (+ (int \0) carry))))
        (let [w (min k (- n base))
              solve (fn [cin]
                      (loop [j 0
                             c cin
                             s (vec (repeat w \0))]
                        (if (>= j w)
                          [s c]
                          (let [x (- (int (nth pa (- n 1 (+ base j)))) (int \0))
                                y (- (int (nth pb (- n 1 (+ base j)))) (int \0))
                                bit (+ x y c)]
                            (recur (inc j)
                                   (bit-shift-right bit 1)
                                   (assoc s (- w 1 j)
                                          (char (+ (int \0) (bit-and bit 1)))))))))
              [s0 c0] (solve 0)
              [s1 c1] (solve 1)
              sel (if (zero? carry) s0 s1)]
          (recur (+ base k)
                 (if (zero? carry) c0 c1)
                 (reduce (fn [o j] (assoc o (+ (- n base w) 1 j) (nth sel j)))
                         out (range w))))))))

(defn -main
  "Read two bit strings, print the (n+1)-bit carry-select sum."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 2)
      (print (add-bits (nth toks 0) (nth toks 1))))))

(-main (slurp *in*))
