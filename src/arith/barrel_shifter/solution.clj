;; barrel shifter: one-hot select muxes each output bit in one pass,
;; replicating the MSB (two's-complement arithmetic right shift).
(require '[clojure.string :as str])

(defn barrel-shift [data hot]
  (let [n (count data)
        k (loop [i 0
                 k 0]
            (if (>= i (count hot))
              k
              (recur (inc i)
                     (if (= (nth hot (- (count hot) 1 i)) \1) i k))))
        sign (nth data 0)]
    (apply str (map #(if (>= % k) (nth data (- % k)) sign) (range n)))))

(defn -main
  "Read data bits and a one-hot token, print the shifted data."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 2)
      (print (barrel-shift (nth toks 0) (nth toks 1))))))

(-main (slurp *in*))
