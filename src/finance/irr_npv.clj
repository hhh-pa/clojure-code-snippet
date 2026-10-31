;; irr_npv: capital budgeting — NPV at the hurdle rate, IRR by bisection
;; on [-0.9999, 10] (NONE without a sign change) and WACC = w_e r_e +
;; w_d r_d (1 - tax). Batch 6 (T385). Input: rate m, cashflows, tax we re
;; wd rd.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [rate (Double/parseDouble (nth toks 0)) m (Long/parseLong (nth toks 1))
            cf (mapv (fn [k] (Double/parseDouble (nth toks (+ 2 k)))) (range m))
            npv (fn [r] (reduce + (map (fn [t] (* (nth cf t) (Math/pow (+ 1.0 r) (- t)))) (range m))))
            N (npv rate)
            lo -0.9999 hi 10.0 flo (npv lo) fhi (npv hi)
            has (or (zero? flo) (zero? fhi) (not= (< flo 0) (< fhi 0)))
            irr (cond (zero? flo) lo (zero? fhi) hi (not has) nil
                      :else (let [[l h] (loop [it 0 l lo h hi fl flo fh fhi]
                                          (if (>= it 200) [l h]
                                            (let [mid (/ (+ l h) 2.0) fm (npv mid)]
                                              (cond (zero? fm) [mid mid]
                                                    (not= (< fm 0) (< fl 0)) (recur (inc it) l mid fl fm)
                                                    :else (recur (inc it) mid h fm fh)))))]
                              (/ (+ l h) 2.0)))
            q (+ 2 m)
            tax (Double/parseDouble (nth toks q)) we (Double/parseDouble (nth toks (+ q 1)))
            re (Double/parseDouble (nth toks (+ q 2))) wd (Double/parseDouble (nth toks (+ q 3)))
            rd (Double/parseDouble (nth toks (+ q 4)))
            wacc (+ (* we re) (* wd rd (- 1.0 tax)))]
        (println (str "NPV: " (fmt "%.6f" N)))
        (println (str "IRR: " (if (nil? irr) "NONE" (fmt "%.6f" irr))))
        (println (str "WACC: " (fmt "%.6f" wacc)))))))
(-main (slurp *in*))
