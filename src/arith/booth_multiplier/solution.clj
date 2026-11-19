;; Booth multiplier: modified radix-4 recoding of the multiplier
;; (Table 11.2) selects 0, +M, +2M, -M or -2M per 3-bit group.
(require '[clojure.string :as str])

(defn negate-bits [bits]
  (let [inv (mapv #(- 1 %) bits)]
    (loop [i 0
           carry 1
           acc inv]
      (if (>= i (count acc))
        acc
        (let [bit (+ (nth acc i) carry)]
          (recur (inc i)
                 (bit-shift-right bit 1)
                 (assoc acc i (bit-and bit 1))))))))

(defn add-bits [a b]
  (loop [i 0
         carry 0
         acc (vec (repeat (count a) 0))]
    (if (>= i (count a))
      acc
      (let [bit (+ (nth a i) (nth b i) carry)]
        (recur (inc i)
               (bit-shift-right bit 1)
               (assoc acc i (bit-and bit 1)))))))

(defn booth-mult [a b]
  (let [n (max (count a) (count b))
        pa (str (str/join (repeat (- n (count a)) \0)) a)
        pb (str (str/join (repeat (- n (count b)) \0)) b)
        m (mapv #(- (int (nth pa (- n 1 %))) (int \0)) (range n))
        y (mapv #(- (int (nth pb (- n 1 %))) (int \0)) (range n))
        width (+ (* 2 n) 2)
        groups (quot (+ n 2) 2)
        bit-at (fn [v j] (if (and (>= j 0) (< j n)) (nth v j) 0))]
    (loop [k 0
           acc (vec (repeat width 0))]
      (if (>= k groups)
        (apply str (map #(char (+ (int \0) %)) (reverse (subvec acc 0 (* 2 n)))))
        (let [sel (+ (bit-shift-left (bit-at y (+ (* 2 k) 1)) 2)
                     (bit-shift-left (bit-at y (* 2 k)) 1)
                     (bit-at y (dec (* 2 k))))]
          (if (or (zero? sel) (= sel 7))
            (recur (inc k) acc)
            (let [off (if (or (= sel 3) (= sel 4)) (inc (* 2 k)) (* 2 k))
                  pp (reduce (fn [v i] (assoc v (+ off i) (nth m i)))
                             (vec (repeat width 0))
                             (range n))
                  pp (if (>= sel 4) (negate-bits pp) pp)]
              (recur (inc k) (add-bits acc pp)))))))))

(defn -main
  "Read two bit strings, print the 2n-bit Booth-multiplier product."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 2)
      (print (booth-mult (nth toks 0) (nth toks 1))))))

(-main (slurp *in*))
