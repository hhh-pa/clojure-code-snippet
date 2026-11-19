;; Pearson + Spearman correlation (Think Stats 2e ch 7).
(require '[clojure.string :as str])
(import '[java.util Locale])

(defn- pearson
  [xs ys]
  (let [n (count xs)
        mx (/ (reduce + xs) n)
        my (/ (reduce + ys) n)
        acc (reduce (fn [[c vx vy] [x y]]
                      (let [dx (- x mx) dy (- y my)]
                        [(+ c (* dx dy)) (+ vx (* dx dx)) (+ vy (* dy dy))]))
                    [0.0 0.0 0.0]
                    (map vector xs ys))
        [c vx vy] acc]
    (if (or (== vx 0.0) (== vy 0.0))
      0.0
      (/ c (Math/sqrt (* vx vy))))))

(defn- midranks
  [xs]
  (let [n (count xs)
        order (vec (sort-by #(nth xs %) (range n)))]
    (loop [i 0 ranks (vec (repeat n 0.0))]
      (if (>= i n)
        ranks
        (let [j (loop [j i]
                  (if (and (< (inc j) n) (== (nth xs (nth order (inc j))) (nth xs (nth order i))))
                    (recur (inc j))
                    j))
              r (/ (+ i j 2) 2.0)
              ranks2 (reduce (fn [rk k] (assoc rk (nth order k) r)) ranks (range i (inc j)))]
          (recur (inc j) ranks2))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          n (Long/parseLong (nth toks 0))
          xs (mapv #(double (Long/parseLong (nth toks (+ 1 (* 2 %))))) (range n))
          ys (mapv #(double (Long/parseLong (nth toks (+ 2 (* 2 %))))) (range n))
          r (pearson xs ys)
          rho (pearson (midranks xs) (midranks ys))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (print (str "PEARSON " (fmt r) "\nSPEARMAN " (fmt rho))))))
