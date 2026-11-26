;; linear probing: insert keys (floored mod hash), print table with _ holes.
(require '[clojure.string :as str])

(defn -main
  "Read m, n, keys and print the probed table."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))
        nums (mapv #(Long/parseLong %) toks)]
    (when (and (>= (count nums) 2) (> (nth nums 0) 0) (>= (nth nums 1) 0))
      (let [m (nth nums 0)
            keys (take (nth nums 1) (drop 2 nums))
            table (reduce
                   (fn [t k]
                     (let [h (mod k m)]
                       (or (some (fn [i]
                                   (when (nil? (t (mod (+ h i) m)))
                                     (assoc t (mod (+ h i) m) k)))
                                 (range m))
                           t)))
                   (vec (repeat m nil))
                   keys)]
        (print (str/join " " (map #(if (nil? %) "_" %) table)))))))

(-main (slurp *in*))
