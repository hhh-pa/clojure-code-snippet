;; comparator: subtractor-derived A>=B (carry-out) and A==B (zero
;; difference); A<=B runs the same test with swapped operands.
(require '[clojure.string :as str])

(defn subtract-bits [a b]
  (loop [i 0
         carry 1
         diff []]
    (if (>= i (count a))
      [diff carry]
      (let [bit (+ (nth a i) (- 1 (nth b i)) carry)]
        (recur (inc i)
               (bit-shift-right bit 1)
               (conj diff (bit-and bit 1)))))))

(defn compare-bits [a b]
  (let [n (max (count a) (count b))
        pa (str (str/join (repeat (- n (count a)) \0)) a)
        pb (str (str/join (repeat (- n (count b)) \0)) b)
        va (mapv #(- (int (nth pa (- n 1 %))) (int \0)) (range n))
        vb (mapv #(- (int (nth pb (- n 1 %))) (int \0)) (range n))
        [diff ge] (subtract-bits va vb)
        [_ le] (subtract-bits vb va)
        eq (if (every? zero? diff) 1 0)]
    (str ge eq le)))

(defn -main
  "Read two bit strings, print ge/eq/le as three 0/1 characters."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 2)
      (print (compare-bits (nth toks 0) (nth toks 1))))))

(-main (slurp *in*))
