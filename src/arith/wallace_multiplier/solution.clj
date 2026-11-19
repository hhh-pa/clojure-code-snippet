;; Wallace multiplier: AND partial-product matrix reduced with 3:2
;; full-adder compressors down to two rows, finished by one ripple add.
(require '[clojure.string :as str])

(defn wallace-mult [a b]
  (let [n (max (count a) (count b))
        pa (str (str/join (repeat (- n (count a)) \0)) a)
        pb (str (str/join (repeat (- n (count b)) \0)) b)
        width (+ (* 2 n) 2)
        cols (loop [j 0
                    cols (vec (repeat width []))]
               (if (>= j n)
                 cols
                 (recur (inc j)
                        (if (= (nth pb (- n 1 j)) \1)
                          (reduce (fn [c i]
                                    (update c (+ i j) conj
                                            (- (int (nth pa (- n 1 i))) (int \0))))
                                  cols
                                  (range n))
                          cols))))]
    (loop [cols cols]
      (if (<= (apply max (map count cols)) 2)
        (loop [c 0
               carry 0
               out ()]
          (if (>= c (* 2 n))
            (apply str out)
            (let [bits (nth cols c)
                  bit (+ carry
                         (if (>= (count bits) 1) (nth bits 0) 0)
                         (if (>= (count bits) 2) (nth bits 1) 0))]
              (recur (inc c)
                     (bit-shift-right bit 1)
                     (conj out (char (+ (int \0) (bit-and bit 1))))))))
        (recur
         (loop [c 0
                nxt (vec (repeat width []))]
           (if (>= c (dec width))
             (update nxt (dec width) into (nth cols (dec width)))
             (let [bits (nth cols c)
                   triples (quot (count bits) 3)]
               (recur (inc c)
                      (loop [t 0
                             nxt nxt]
                        (if (>= t triples)
                          (update nxt c into (subvec bits (* 3 triples)))
                          (let [x (nth bits (* 3 t))
                                yy (nth bits (+ (* 3 t) 1))
                                z (nth bits (+ (* 3 t) 2))]
                            (recur (inc t)
                                   (-> nxt
                                       (update c conj (bit-xor x (bit-xor yy z)))
                                       (update (inc c) conj
                                               (bit-shift-right (+ x yy z) 1))))))))))))))))

(defn -main
  "Read two bit strings, print the 2n-bit Wallace-multiplier product."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 2)
      (print (wallace-mult (nth toks 0) (nth toks 1))))))

(-main (slurp *in*))
