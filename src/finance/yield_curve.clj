;; yield_curve: zero-coupon yield curve bootstrapping (semiannual par
;; curve at consecutive half-years) & forward rate extraction. Discount
;; factors from par bonds, zeros as 2*(df^-1/j - 1), annualized 6M
;; forwards 2*(df_{j-1}/df_j - 1). Batch 6 (T379). Input: n, par yields.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [n (Long/parseLong (nth toks 0))
            y (mapv (fn [k] (Double/parseDouble (nth toks (+ 1 k)))) (range n))
            df (loop [j 0 df (vec (repeat n 0.0))]
                 (if (>= j n) df
                   (let [cp (* (nth y j) 50.0)
                         acc (reduce + 0.0 (map (fn [k] (* cp (nth df k))) (range j)))]
                     (recur (inc j) (assoc df j (/ (- 100.0 acc) (+ 100.0 cp)))))))]
        (println (str "ZEROS:" (apply str (map (fn [j] (str " " (fmt "%.6f" (* 2.0 (- (Math/pow (nth df j) (/ -1.0 (inc j))) 1.0)))))
                                               (range n)))))
        (println (str "FORWARDS:" (apply str (map (fn [j] (str " " (fmt "%.6f" (* 2.0 (- (/ (nth df (dec j)) (nth df j)) 1.0)))))
                                                  (range 1 n)))))))))
(-main (slurp *in*))
