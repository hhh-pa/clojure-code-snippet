;; fractional_diff: fixed-width window fractional differentiation (AFML
;; ch.5). Weights w_0 = 1, w_k = -w_{k-1} (d-k+1)/k; output y_t is the
;; width-L weighted sum of x_{t-k}. Batch 6 (T368). Input: d L M, series.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [d (Double/parseDouble (nth toks 0)) L (Long/parseLong (nth toks 1))
            M (Long/parseLong (nth toks 2))
            x (mapv (fn [k] (Double/parseDouble (nth toks (+ 3 k)))) (range M))
            w (loop [k 1 w (assoc (vec (repeat L 0.0)) 0 1.0)]
                (if (>= k L) w (recur (inc k) (assoc w k (/ (* (- (nth w (dec k))) (+ (- d k) 1.0)) k)))))]
        (println (str "W:" (apply str (map (fn [v] (str " " (fmt "%.6f" v))) w))))
        (println (str "Y:" (apply str (map (fn [t] (str " " (fmt "%.6f" (reduce + (map (fn [k] (* (nth w k) (nth x (- t k)))) (range L))))))
                                           (range (dec L) M)))))))))
(-main (slurp *in*))
