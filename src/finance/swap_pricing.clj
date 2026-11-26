;; Par swap rate + PV01 (FE ch 5).
(require '[clojure.string :as str])
(import '[java.util Locale])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          n (Long/parseLong (nth toks 0))
          [pv01 dfn] (reduce (fn [[pp _] i]
                               (let [tau (Double/parseDouble (nth toks (+ 1 (* 2 i))))
                                     df (Double/parseDouble (nth toks (+ 2 (* 2 i))))]
                                 [(+ pp (* tau df)) df]))
                             [0.0 0.0] (range n))
          rate (/ (- 1 dfn) pv01)
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (print (str "SWAPRATE " (fmt rate) "\nPV01 " (fmt pv01))))))
