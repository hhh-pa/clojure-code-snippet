;; kogge-stone adder: parallel-prefix carry with the o operator.
(require '[clojure.string :as str])

(defn add-bits [a b]
  (let [n (max (count a) (count b))
        pa (str (str/join (repeat (- n (count a)) \0)) a)
        pb (str (str/join (repeat (- n (count b)) \0)) b)
        bit (fn [s i] (- (int (nth s (- n 1 i))) (int \0)))
        op (fn [[g p] [g2 p2]] [(if (or (= g 1) (and (= p 1) (= g2 1))) 1 0)
                                (if (and (= p 1) (= p2 1)) 1 0)])
        init (mapv (fn [i] [(bit-and (bit pa i) (bit pb i))
                            (bit-xor (bit pa i) (bit pb i))])
                   (range n))
        pref (loop [gp init
                    stride 1]
               (if (>= stride n)
                 gp
                 (recur (mapv (fn [i]
                                (if (< i stride)
                                  (nth gp i)
                                  (op (nth gp i) (nth gp (- i stride)))))
                              (range n))
                        (* stride 2))))
        carries (mapv first pref)]
    (apply str (nth carries (dec n))
           (map (fn [i]
                  (let [c (if (zero? i) 0 (nth carries (dec i)))]
                    (bit-xor (bit-xor (bit a i) (bit b i)) c)))
                (range (dec n) -1 -1)))))

(defn -main
  "Read two bit strings, print the (n+1)-bit Kogge-Stone sum."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 2)
      (print (add-bits (nth toks 0) (nth toks 1))))))

(-main (slurp *in*))
