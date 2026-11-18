;; Gaussian kernel density estimation (Think Stats 2e ch 6).
(require '[clojure.string :as str])
(import '[java.util Locale])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          n (Long/parseLong (nth toks 0))
          xs (mapv #(Double/parseDouble %) (subvec toks 1 (inc n)))
          h (Double/parseDouble (nth toks (inc n)))
          m (Long/parseLong (nth toks (+ 2 n)))
          qs (mapv #(Double/parseDouble %) (subvec toks (+ 3 n) (+ 3 n m)))
          coef (/ 1 (* h (Math/sqrt (* 2 Math/PI))))
          denom (* (* 2 h) h)
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))
          out (mapv (fn [q]
                      (let [sum (reduce + (map (fn [x]
                                                 (let [d (- q x)]
                                                   (Math/exp (- (/ (* d d) denom)))))
                                               xs))]
                        (fmt (/ (* coef sum) n))))
                    qs)]
      (print (str/join "\n" out)))))
