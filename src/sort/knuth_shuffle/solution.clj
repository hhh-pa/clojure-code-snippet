;; https://leetcode.com/problems/shuffle-an-array/
;; knuth-shuffle: Fisher-Yates shuffle with a specified LCG so every
;; language agrees bit-for-bit. state = (1103515245 * state + 12345)
;; mod 2^31; for i from n-1 down to 1, j = state mod (i+1), swap.
;; The driver reads seed, n, values from stdin and prints [a,b,c].
(require '[clojure.string :as str])

(defn -main
  "Read seed, n, values from input and print the shuffled vector."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))
        nums (mapv #(Long/parseLong %) toks)]
    (when (and (>= (count nums) 2) (>= (nth nums 1) 0))
      (let [seed (nth nums 0)
            n (nth nums 1)
            a (vec (take n (drop 2 nums)))]
        (loop [v a
               state (rem seed 2147483648)
               i (dec (count a))]
          (if (< i 1)
            (print (str "[" (str/join "," v) "]"))
            (let [ns (rem (+ (* 1103515245 state) 12345) 2147483648)
                  j (rem ns (inc i))]
              (recur (assoc v i (v j) j (v i)) ns (dec i)))))))))

(-main (slurp *in*))
