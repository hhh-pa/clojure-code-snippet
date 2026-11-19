;; Probability of backtest overfitting (AFML ch 11).
(require '[clojure.string :as str])
(import '[java.util Locale])

(defn- sharpe
  [vals]
  (let [n (count vals)]
    (if (< n 2)
      0.0
      (let [mean (/ (reduce + vals) n)
            tot (reduce + (map (fn [v] (let [d (- v mean)] (* d d))) vals))
            std (Math/sqrt (/ tot (dec n)))]
        (if (== std 0.0) 0.0 (/ mean std))))))

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

(defn- combos
  [s kk]
  (loop [combo (vec (range kk)) acc []]
    (let [acc2 (conj acc combo)
          p (loop [pp (dec kk)]
              (cond (< pp 0) -1
                    (not (== (nth combo pp) (+ (- s kk) pp))) pp
                    :else (recur (dec pp))))]
      (if (< p 0)
        acc2
        (recur (reduce (fn [cc i] (assoc cc i (inc (nth cc (dec i)))))
                       (assoc combo p (inc (nth combo p)))
                       (range (inc p) kk))
               acc2)))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          t (Long/parseLong (nth toks 0))
          n (Long/parseLong (nth toks 1))
          ss (Long/parseLong (nth toks 2))
          mat (mapv (fn [i] (mapv #(Double/parseDouble (nth toks (+ 3 (* i n) %))) (range n)))
                    (range t))
          q (quot t ss)
          rem (mod t ss)
          bounds (vec (reductions (fn [acc g] (+ acc q (if (< g rem) 1 0))) 0 (range ss)))
          med (/ (+ n 1) 2.0)
          [bad total] (reduce (fn [[bd tt] combo]
                                (let [inset (set combo)
                                      [isr oosr]
                                      (reduce (fn [[ir orr] g]
                                                (let [rows (range (nth bounds g) (nth bounds (inc g)))]
                                                  (if (contains? inset g)
                                                    [(into ir rows) orr]
                                                    [ir (into orr rows)])))
                                              [[] []] (range ss))
                                      is-sh (mapv (fn [c] (sharpe (mapv #(nth (nth mat %) c) isr)))
                                                  (range n))
                                      oos-sh (mapv (fn [c] (sharpe (mapv #(nth (nth mat %) c) oosr)))
                                                   (range n))
                                      is-rk (midranks is-sh)
                                      oos-rk (midranks oos-sh)
                                      sel (filterv #(> (nth is-rk %) med) (range n))
                                      w (if (empty? sel)
                                          0.5
                                          (/ (/ (reduce + (map #(nth oos-rk %) sel)) (count sel))
                                             (+ n 1)))]
                                  [(if (< w 0.5) (inc bd) bd) (inc tt)]))
                              [0 0] (combos ss (quot ss 2)))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (print (str "PBO " (fmt (/ (double bad) total)))))))
