;; CAPM beta + expected return (Gestion ch 2).
(require '[clojure.string :as str])
(import '[java.util Locale])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          n (Long/parseLong (nth toks 0))
          ris (mapv #(Double/parseDouble (nth toks (+ 1 (* 2 %)))) (range n))
          rms (mapv #(Double/parseDouble (nth toks (+ 2 (* 2 %)))) (range n))
          rf (Double/parseDouble (nth toks (+ 1 (* 2 n))))
          mi (/ (reduce + ris) n)
          mm (/ (reduce + rms) n)
          [c vm] (reduce (fn [[cc vv] [ri rm]]
                           (let [di (- ri mi) dm (- rm mm)]
                             [(+ cc (* di dm)) (+ vv (* dm dm))]))
                         [0.0 0.0] (map vector ris rms))
          beta (if (== vm 0.0) 0.0 (/ c vm))
          exp (+ rf (* beta (- mm rf)))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (print (str "BETA " (fmt beta) "\nEXP " (fmt exp))))))
