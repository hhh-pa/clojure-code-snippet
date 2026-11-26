;; Gaussian kernel density estimation (Think Stats 2e ch 6).
(require '[clojure.string :as str])

;; Six-decimal formatting in pure Clojure (String/format varargs
;; mis-formats tiny doubles under clojure-rs; this matches %.6f).
(defn fmt6 [v]
  (let [neg (neg? v)
        x (if neg (- v) v)
        scaled (long (+ (* x 1000000.0) 0.5))
        ip (quot scaled 1000000)
        fp (rem scaled 1000000)]
    (str (when neg "-") ip "." (subs (str (+ 1000000 fp)) 1))))

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
          out (mapv (fn [q]
                      (let [sum (reduce + (map (fn [x]
                                                 (let [d (- q x)]
                                                   (Math/exp (- (/ (* d d) denom)))))
                                               xs))]
                        (fmt6 (/ (* coef sum) n))))
                    qs)]
      (print (str/join "\n" out)))))
