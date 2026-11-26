;; CDS hazard bootstrap + par spread (FE ch 16).
(require '[clojure.string :as str])
(import '[java.util Locale])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          rec (Double/parseDouble (nth toks 0))
          r (Double/parseDouble (nth toks 1))
          n (Long/parseLong (nth toks 2))
          ts (mapv #(Double/parseDouble (nth toks (+ 3 (* 2 %)))) (range n))
          ds (mapv #(Double/parseDouble (nth toks (+ 4 (* 2 %)))) (range n))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))
          [hs prot prem] (loop [i 0 pt 0.0 ps 1.0 pr 0.0 pm 0.0 acc []]
                           (if (>= i n)
                             [acc pr pm]
                             (let [df (Math/exp (* (- r) (nth ts i)))
                                   sv (/ (nth ds i) df)
                                   lam (/ (- (Math/log (/ sv ps))) (- (nth ts i) pt))]
                               (recur (inc i) (nth ts i) sv
                                      (+ pr (* df (- ps sv)))
                                      (+ pm (* (* (- (nth ts i) pt) df) sv))
                                      (conj acc (fmt lam))))))
          spread (/ (* prot (- 1 rec)) prem)]
      (print (str "HAZARDS " (str/join " " hs)
                  "\nSPREAD " (fmt spread)
                  "\nSPREAD_BPS " (fmt (* spread 10000)))))))
