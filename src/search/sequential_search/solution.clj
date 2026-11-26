;; sequential search: first index of target, or -1.
(require '[clojure.string :as str])

(defn -main
  "Read n, values, target from input and print the first match index."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))
        nums (mapv #(Long/parseLong %) toks)]
    (when (and (>= (count nums) 2) (>= (nth nums 0) 0))
      (let [n (nth nums 0)]
        (when (>= (count nums) (+ 2 n))
          (let [target (nth nums (+ 1 n))
                ans (first (keep-indexed #(when (= %2 target) %1)
                                         (take n (drop 1 nums))))]
            (print (if (nil? ans) -1 ans))))))))

(-main (slurp *in*))
