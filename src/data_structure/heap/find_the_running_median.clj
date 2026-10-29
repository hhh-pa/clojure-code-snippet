(let [s (slurp *in*)]
  (when-not (clojure.string/blank? s)
    (let [toks (.split (.trim ^String s) "\\s+")
          n (Integer/parseInt (aget ^objects toks 0))
          max-heap (java.util.PriorityQueue. 1024 (java.util.Collections/reverseOrder))
          min-heap (java.util.PriorityQueue. 1024)
          out (StringBuilder.)
          fmt (java.text.DecimalFormat. "0.0")]
      (dotimes [i n]
        (let [v (Integer/parseInt (aget ^objects toks (inc i)))]
          (if (or (.isEmpty max-heap) (<= v (int (.peek max-heap))))
            (.add max-heap v)
            (.add min-heap v))
          ;; Rebalance
          (while (> (.size max-heap) (inc (.size min-heap)))
            (.add min-heap (.poll max-heap)))
          (while (> (.size min-heap) (.size max-heap))
            (.add max-heap (.poll min-heap)))
          (let [med (if (> (.size max-heap) (.size min-heap))
                      (double (int (.peek max-heap)))
                      (/ (+ (double (int (.peek max-heap))) (double (int (.peek min-heap)))) 2.0))]
            (.append out (String/format java.util.Locale/US "%.1f" (to-array [med])))
            (.append out \newline))))
      (print (str out)))))
