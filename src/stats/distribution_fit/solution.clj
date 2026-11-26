;; Distribution fitting (Think Stats 2e ch 5).
(require '[clojure.string :as str])
(import '[java.util Locale])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (str/split (str/trim s) #"\s+")
          n (Long/parseLong (nth toks 0))
          nb (Long/parseLong (nth toks 1))
          xs (mapv #(double (Long/parseLong %)) (subvec (vec toks) 2 (+ 2 n)))
          mean (/ (reduce + xs) n)
          variance (/ (reduce + (map (fn [x] (let [d (- x mean)] (* d d))) xs)) (dec n))
          logs (mapv #(Math/log10 %) xs)
          lmean (/ (reduce + logs) n)
          lvar (/ (reduce + (map (fn [x] (let [d (- x lmean)] (* d d))) logs)) (dec n))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (print (str/join "\n" [(str "BINOMIAL " (fmt (/ mean nb)))
                             (str "POISSON " (fmt mean))
                             (str "EXPONENTIAL " (fmt (/ 1 mean)))
                             (str "NORMAL " (fmt mean) " " (fmt (Math/sqrt variance)))
                             (str "LOGNORMAL " (fmt lmean) " " (fmt (Math/sqrt lvar)))])))))
