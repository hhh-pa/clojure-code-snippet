;; Cost-of-carry forwards + FRA valuation (Gestion ch 10, FE ch 3-4).
(require '[clojure.string :as str])
(import '[java.util Locale])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          f (mapv #(Double/parseDouble %) toks)
          eqf (* (nth f 0) (Math/exp (* (- (nth f 1) (nth f 2)) (nth f 3))))
          fxf (* (nth f 4) (Math/exp (* (- (nth f 5) (nth f 6)) (nth f 7))))
          commf (* (nth f 8) (Math/exp (* (- (+ (nth f 9) (nth f 10)) (nth f 11)) (nth f 12))))
          frav (* (* (* (nth f 13) (nth f 14)) (- (nth f 15) (nth f 16))) (nth f 17))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (print (str "EQF " (fmt eqf) "\nFXF " (fmt fxf)
                  "\nCOMMF " (fmt commf) "\nFRAV " (fmt frav))))))
