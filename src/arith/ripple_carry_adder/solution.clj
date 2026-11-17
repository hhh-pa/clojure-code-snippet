;; ripple-carry adder: sequential carry chain over two bit strings.
(require '[clojure.string :as str])

(defn add-bits [a b]
  (let [n (max (count a) (count b))
        pa (str (str/join (repeat (- n (count a)) \0)) a)
        pb (str (str/join (repeat (- n (count b)) \0)) b)]
    (loop [i (dec n)
           carry 0
           out ()]
      (if (neg? i)
        (str (char (+ (int \0) carry)) (apply str out))
        (let [bit (+ (- (int (nth pa i)) (int \0))
                     (- (int (nth pb i)) (int \0))
                     carry)]
          (recur (dec i)
                 (bit-shift-right bit 1)
                 (conj out (char (+ (int \0) (bit-and bit 1))))))))))

(defn -main
  "Read two bit strings, print the (n+1)-bit ripple-carry sum."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 2)
      (print (add-bits (nth toks 0) (nth toks 1))))))

(-main (slurp *in*))
