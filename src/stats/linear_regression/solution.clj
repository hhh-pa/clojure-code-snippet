;; Least squares fit + R^2 (Think Stats 2e ch 10).
(require '[clojure.string :as str])
(import '[java.util Locale])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          n (Long/parseLong (nth toks 0))
          xs (mapv #(Long/parseLong (nth toks (+ 1 (* 2 %)))) (range n))
          ys (mapv #(Long/parseLong (nth toks (+ 2 (* 2 %)))) (range n))
          mx (/ (reduce + (map double xs)) n)
          my (/ (reduce + (map double ys)) n)
          [c vx] (reduce (fn [[cc vv] [x y]]
                           (let [dx (- (double x) mx) dy (- (double y) my)]
                             [(+ cc (* dx dy)) (+ vv (* dx dx))]))
                         [0.0 0.0] (map vector xs ys))
          [slope inter r2] (if (== vx 0.0)
                             [0.0 my 0.0]
                             (let [slope (/ c vx)
                                   inter (- my (* slope mx))
                                   [ssres sstot]
                                   (reduce (fn [[rr tt] [x y]]
                                            (let [pred (+ (* slope (double x)) inter)
                                                  r (- (double y) pred)
                                                  d (- (double y) my)]
                                              [(+ rr (* r r)) (+ tt (* d d))]))
                                          [0.0 0.0] (map vector xs ys))]
                               [slope inter (if (== sstot 0.0) 0.0 (- 1 (/ ssres sstot)))]))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (print (str "SLOPE " (fmt slope) "\nINTER " (fmt inter) "\nRSQ " (fmt r2))))))
