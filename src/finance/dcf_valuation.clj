;; DCF + Gordon DDM valuation (Gestion ch 7-9).
(require '[clojure.string :as str])
(import '[java.util Locale])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          r (Double/parseDouble (nth toks 0))
          g (Double/parseDouble (nth toks 1))
          n (Long/parseLong (nth toks 2))
          fcfs (mapv #(Double/parseDouble %) (subvec toks 3 (+ 3 n)))
          d1 (Double/parseDouble (nth toks (+ 3 n)))
          [df pv] (reduce (fn [[dd pp] fcf]
                            (let [nd (* dd (+ 1 r))]
                              [nd (+ pp (/ fcf nd))]))
                          [1.0 0.0] fcfs)
          tv (/ (* (last fcfs) (+ 1 g)) (- r g))
          dcf (+ pv (/ tv df))
          ddm (/ d1 (- r g))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (print (str "DCF " (fmt dcf) "\nDDM " (fmt ddm))))))
