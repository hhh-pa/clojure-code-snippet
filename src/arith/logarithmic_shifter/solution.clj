;; logarithmic shifter: one stage per amount bit; stage j shifts by
;; 2^j or passes through (binary-encoded control, no decoder).
(require '[clojure.string :as str])

(defn log-shift [data amt]
  (let [n (count data)]
    (loop [j 0
           step 1
           cur (vec data)]
      (if (>= j (count amt))
        (apply str cur)
        (recur (inc j)
               (if (< step n) (* step 2) step)
               (if (= (nth amt (- (count amt) 1 j)) \1)
                 (if (>= step n)
                   (vec (repeat n (nth cur 0)))
                   (mapv #(if (>= % step) (nth cur (- % step)) (nth cur 0))
                         (range n)))
                 cur))))))

(defn -main
  "Read data bits and a binary amount, print the shifted data."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 2)
      (print (log-shift (nth toks 0) (nth toks 1))))))

(-main (slurp *in*))
