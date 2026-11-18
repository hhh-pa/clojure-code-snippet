;; Breeden-Litzenberger density + CDF (FE ch 15).
(require '[clojure.string :as str])
(import '[java.util Locale])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (vec (str/split (str/trim s) #"\s+"))
          t (Double/parseDouble (nth toks 0))
          r (Double/parseDouble (nth toks 1))
          m (Long/parseLong (nth toks 2))
          ks (mapv #(Double/parseDouble (nth toks (+ 3 (* 2 %)))) (range m))
          calls (mapv #(Double/parseDouble (nth toks (+ 4 (* 2 %)))) (range m))
          h (- (nth ks 1) (nth ks 0))
          disc (Math/exp (* r t))
          fmt (fn [v] (String/format Locale/US "%.6f" (to-array [v])))
          out (mapv (fn [i]
                      (let [d2 (/ (+ (- (nth calls (inc i)) (* 2 (nth calls i)))
                                      (nth calls (dec i)))
                                   (* h h))
                            f (* disc d2)
                            d1 (/ (- (nth calls (inc i)) (nth calls (dec i))) (* 2 h))
                            cdf (+ 1 (* disc d1))]
                        (str (fmt (nth ks i)) " " (fmt f) " " (fmt cdf))))
                    (range 1 (dec m)))]
      (print (str/join "\n" out)))))
