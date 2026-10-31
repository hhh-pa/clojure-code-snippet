;; sadf: Supremum Augmented Dickey-Fuller explosive-bubble test (lag 0).
;; For each expanding window [0..e], DF regression dy_t = a + b*y_{t-1};
;; SADF is the supremum of b's t-statistic (0.0 on degenerate windows).
;; Batch 6 (T374). Input: M r0, series.
(require '[clojure.string :as str])
(def separators #"\s+")
(defn tokenize [input] (if (str/blank? input) [] (str/split (str/trim input) separators)))
(defn fmt [s & xs] (String/format java.util.Locale/US s (to-array xs)))
(defn tstat [y e]
  (let [nobs e
        my (/ (reduce + (map #(nth y (dec %)) (range 1 (inc e)))) nobs)
        md (/ (reduce + (map #(double (- (nth y %) (nth y (dec %)))) (range 1 (inc e)))) nobs)
        pairs (map (fn [t] [(- (nth y (dec t)) my) (- (- (nth y t) (nth y (dec t))) md)]) (range 1 (inc e)))
        sxx (reduce + (map (fn [[a _]] (* a a)) pairs))
        sxy (reduce + (map (fn [[a b]] (* a b)) pairs))]
    (if (and (pos? sxx) (> nobs 2))
      (let [b (/ sxy sxx) a (- md (* b my))
            sse (reduce + (map (fn [t] (let [r (- (- (nth y t) (nth y (dec t))) a (* b (nth y (dec t))))] (* r r)))
                               (range 1 (inc e))))
            s2 (/ sse (- nobs 2))]
        (if (pos? s2) (/ b (Math/sqrt (/ s2 sxx))) 0.0)) 0.0)))
(defn -main [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [M (Long/parseLong (nth toks 0)) r0 (Double/parseDouble (nth toks 1))
            y (mapv (fn [k] (Double/parseDouble (nth toks (+ 2 k)))) (range M))
            emin (max 3 (dec (long (* r0 M))))
            res (loop [e emin sadf -1e100 arg emin]
                  (if (>= e M) [sadf arg]
                    (let [t (tstat y e)]
                      (if (> t sadf) (recur (inc e) t e) (recur (inc e) sadf arg)))))]
        (println (str "SADF: " (fmt "%.6f" (nth res 0))))
        (println (str "ARGMAX: " (nth res 1)))))))
(-main (slurp *in*))
