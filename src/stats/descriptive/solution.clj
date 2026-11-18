;; Descriptive statistics + Cohen's d (Think Stats 2e ch 1-2).
(require '[clojure.string :as str])
(import '[java.util Locale])

(defn- desc-stats
  [xs]
  (let [n (count xs)
        mean (/ (reduce + (map double xs)) n)
        tot (reduce + (map (fn [x] (let [d (- (double x) mean)] (* d d))) xs))
        variance (/ tot (dec n))
        srt (sort xs)
        med (if (odd? n)
              (double (nth srt (quot n 2)))
              (/ (+ (nth srt (dec (quot n 2))) (nth srt (quot n 2))) 2.0))]
    [n mean variance (Math/sqrt variance) (first srt) (last srt) med]))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          n1 (nth toks 0)
          g1 (subvec toks 1 (inc n1))
          n2 (nth toks (inc n1))
          g2 (subvec toks (+ 2 n1) (+ 2 n1 n2))
          [n1 m1 v1 s1 mn1 mx1 md1] (desc-stats g1)
          [n2 m2 v2 s2 mn2 mx2 md2] (desc-stats g2)
          t1 (* n1 v1)
          t2 (* n2 v2)
          pv (/ (+ t1 t2) (+ n1 n2))
          d (/ (- m1 m2) (Math/sqrt pv))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (print (str n1 " " (fmt m1) " " (fmt v1) " " (fmt s1) " " mn1 " " mx1 " " (fmt md1)
                  "\n" n2 " " (fmt m2) " " (fmt v2) " " (fmt s2) " " mn2 " " mx2 " " (fmt md2)
                  "\nCOHEN " (fmt d))))))
