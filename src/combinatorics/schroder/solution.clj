;; Large Schroder numbers via small-number recurrence (Brualdi 8.5).
(require '[clojure.string :as str])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          q (nth toks 0)
          ns (vec (take q (drop 1 toks)))
          mx (apply max 0 ns)
          s (loop [n 2 sv (assoc (vec (repeat (+ mx 2) 0)) 1 1)]
              (if (> n (inc mx))
                sv
                (recur (inc n)
                       (assoc sv n (- (* 2 (reduce + (map (fn [j] (* (nth sv j) (nth sv (- n j))))
                                                              (range 1 n))))
                                         (nth sv (dec n)))))))]
      (print (str/join "\n" (map (fn [n] (if (zero? n) "1" (str (* 2 (nth s (inc n)))))) ns))))))
