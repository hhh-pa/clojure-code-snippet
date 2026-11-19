;; array multiplier: one AND partial product per multiplier bit,
;; accumulated row by row with ripple-carry addition.
(require '[clojure.string :as str])

(defn add-row [acc a j n]
  (loop [i 0
         carry 0
         acc acc]
    (if (>= i n)
      (loop [pos (+ n j)
             carry carry
             acc acc]
        (if (zero? carry)
          acc
          (let [bit (+ (nth acc pos) carry)]
            (recur (inc pos)
                   (bit-shift-right bit 1)
                   (assoc acc pos (bit-and bit 1))))))
      (let [bit (+ (nth acc (+ i j))
                   (- (int (nth a (- n 1 i))) (int \0))
                   carry)]
        (recur (inc i)
               (bit-shift-right bit 1)
               (assoc acc (+ i j) (bit-and bit 1)))))))

(defn array-mult [a b]
  (let [n (max (count a) (count b))
        pa (str (str/join (repeat (- n (count a)) \0)) a)
        pb (str (str/join (repeat (- n (count b)) \0)) b)]
    (loop [j 0
           acc (vec (repeat (* 2 n) 0))]
      (if (>= j n)
        (apply str (map #(char (+ (int \0) %)) (reverse acc)))
        (recur (inc j)
               (if (= (nth pb (- n 1 j)) \1)
                 (add-row acc pa j n)
                 acc))))))

(defn -main
  "Read two bit strings, print the 2n-bit array-multiplier product."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 2)
      (print (array-mult (nth toks 0) (nth toks 1))))))

(-main (slurp *in*))
