;; Variance-swap replication strike (FE ch 14).
(require '[clojure.string :as str])
(import '[java.util Locale])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          t (Double/parseDouble (nth toks 0))
          r (Double/parseDouble (nth toks 1))
          fwd (Double/parseDouble (nth toks 2))
          m (Long/parseLong (nth toks 3))
          ks (mapv #(Double/parseDouble (nth toks (+ 4 (* 3 %)))) (range m))
          calls (mapv #(Double/parseDouble (nth toks (+ 5 (* 3 %)))) (range m))
          puts (mapv #(Double/parseDouble (nth toks (+ 6 (* 3 %)))) (range m))
          k0 (reduce (fn [acc k] (if (<= k fwd) k acc)) (nth ks 0) ks)
          acc (reduce + (map (fn [i]
                               (let [dk (cond (== i 0) (- (nth ks 1) (nth ks 0))
                                             (== i (dec m)) (- (nth ks (dec m)) (nth ks (- m 2)))
                                             :else (/ (- (nth ks (inc i)) (nth ks (dec i))) 2))
                                     q (cond (< (nth ks i) k0) (nth puts i)
                                             (> (nth ks i) k0) (nth calls i)
                                             :else (/ (+ (nth calls i) (nth puts i)) 2))]
                                 (* (/ dk (* (nth ks i) (nth ks i))) q)))
                             (range m)))
          adj (- (/ fwd k0) 1)
          kvar (- (* (/ (* 2 (Math/exp (* r t))) t) acc) (/ (* adj adj) t))
          vol (Math/sqrt (max kvar 0.0))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))]
      (print (str "VARSTRIKE " (fmt kvar) "\nVOLSTRIKE " (fmt vol))))))
