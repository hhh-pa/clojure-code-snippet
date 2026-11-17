;; brent-kung adder: reduce tree + distribute tree over (g,p) pairs.
(require '[clojure.string :as str])

(defn op [[g p] [g2 p2]]
  [(if (or (= g 1) (and (= p 1) (= g2 1))) 1 0)
   (if (and (= p 1) (= p2 1)) 1 0)])

(defn add-bits [a b]
  (let [n (max (count a) (count b))
        pa (str (str/join (repeat (- n (count a)) \0)) a)
        pb (str (str/join (repeat (- n (count b)) \0)) b)
        bit (fn [s i] (- (int (nth s (- n 1 i))) (int \0)))
        size (loop [s 1] (if (>= s n) s (recur (* s 2))))
        init (mapv (fn [i]
                     (if (< i n)
                       [(bit-and (bit pa i) (bit pb i))
                        (bit-xor (bit pa i) (bit pb i))]
                       [0 1]))
                   (range size))
        up (loop [gp init
                  stride 1]
             (if (>= stride size)
               gp
               (recur (reduce (fn [g i] (assoc g i (op (nth g i) (nth g (- i stride)))))
                              gp (range (dec (* 2 stride)) size (* 2 stride)))
                      (* stride 2))))
        down (loop [gp (assoc up (dec size) [0 1])
                    stride (quot size 2)]
               (if (< stride 1)
                 gp
                 (recur (reduce (fn [g i]
                                  (let [t (nth g (- i stride))]
                                    (-> g
                                        (assoc (- i stride) (nth g i))
                                        (assoc i (op (nth g i) t)))))
                                gp (range (dec (* 2 stride)) size (* 2 stride)))
                        (quot stride 2))))
        excl (mapv first down)]
    (apply str
           (let [g0 (first (nth init (dec n)))
                 p0 (second (nth init (dec n)))]
             (if (or (= g0 1) (and (= p0 1) (= (nth excl (dec n)) 1))) 1 0))
           (map (fn [i]
                  (let [x (bit pa i)
                        y (bit pb i)]
                    (bit-xor (bit-xor x y) (nth excl i))))
                (range (dec n) -1 -1)))))

(defn -main
  "Read two bit strings, print the (n+1)-bit Brent-Kung sum."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 2)
      (print (add-bits (nth toks 0) (nth toks 1))))))

(-main (slurp *in*))
