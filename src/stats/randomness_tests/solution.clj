;; RNG statistical battery: monobit/poker/runs/long-runs/byte cores.

(let [bits (.trim (slurp *in*)) n (count bits)]
  (when (pos? n)
    (let [ones (count (filter #(= % \1) bits))
          nibs (reduce (fn [m i]
                          (update m (reduce (fn [v j] (+ (* v 2) (if (= (nth bits (+ i j)) \1) 1 0))) 0 (range 4)) inc))
                        (vec (repeat 16 0)) (range 0 n 4))
          runs (loop [i 1 r 1 cur (first bits) cl 1 lr 0]
                 (if (>= i n) [r (if (= cur \1) (max lr cl) lr)]
                     (let [b (nth bits i)]
                       (if (= b cur) (recur (inc i) r cur (inc cl) lr)
                           (recur (inc i) (inc r) b 1 (if (= cur \1) (max lr cl) lr))))))
          bc (reduce (fn [m i]
                       (update m (reduce (fn [v j] (+ (* v 2) (if (= (nth bits (+ i j)) \1) 1 0))) 0 (range 8)) inc))
                     (vec (repeat 256 0)) (range 0 n 8))]
      (print (str n " " ones " " (reduce + 0 (map #(* % %) nibs)) " " (first runs) " " (second runs) " "
                  (reduce + 0 (map #(* % %) bc)))))))
