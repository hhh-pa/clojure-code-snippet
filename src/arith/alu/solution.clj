;; mini ALU: mux-selected datapath over a shared bit-level adder
;; (ADD/SUB via B-invert + carry-in), a logic unit, and a shifter.
(require '[clojure.string :as str])

(defn add-sub [a b sub]
  (loop [i 0
         carry (if sub 1 0)
         out []]
    (if (>= i (count a))
      [out carry]
      (let [bb (if sub (- 1 (nth b i)) (nth b i))
            bit (+ (nth a i) bb carry)]
        (recur (inc i)
               (bit-shift-right bit 1)
               (conj out (bit-and bit 1)))))))

(defn parse-amount [b n]
  (reduce (fn [k c] (min n (+ (* k 2) (- (int c) (int \0))))) 0 b))

(defn execute [op ab bb b n]
  (cond
    (or (= op "ADD") (= op "SUB")) (add-sub ab bb (= op "SUB"))
    (or (= op "AND") (= op "OR") (= op "XOR"))
    [(mapv #(cond (= op "AND") (bit-and %1 %2)
                  (= op "OR") (bit-or %1 %2)
                  :else (bit-xor %1 %2))
            ab bb)
     0]
    :else
    (let [k (parse-amount b n)]
      (if (= op "SHL")
        [(mapv #(if (>= % k) (nth ab (- % k)) 0) (range n)) 0]
        (let [sign (nth ab (dec n))]
          [(mapv #(if (< (+ % k) n) (nth ab (+ % k)) sign) (range n)) 0])))))

(defn alu [op a b]
  (let [n (max (count a) (count b))
        pa (str (str/join (repeat (- n (count a)) \0)) a)
        pb (str (str/join (repeat (- n (count b)) \0)) b)
        va (mapv #(- (int (nth pa (- n 1 %))) (int \0)) (range n))
        vb (mapv #(- (int (nth pb (- n 1 %))) (int \0)) (range n))
        [res carry] (execute op va vb pb n)
        out (apply str (map #(char (+ (int \0) %)) (reverse res)))
        zero (if (every? zero? res) 1 0)]
    (str out " " carry zero (nth res (dec n)))))

(defn -main
  "Read an opcode and two operands, print the result and CZN flags."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 3)
      (print (alu (nth toks 0) (nth toks 1) (nth toks 2))))))

(-main (slurp *in*))
