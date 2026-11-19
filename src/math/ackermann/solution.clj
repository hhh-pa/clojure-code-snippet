;; Ackermann function (exact closed forms for m <= 3).
(require '[clojure.string :as str])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          m (nth toks 0) n (nth toks 1)]
      (print (cond
               (zero? m) (inc n)
               (= m 1) (+ n 2)
               (= m 2) (+ (* 2 n) 3)
               (= m 3) (- (bit-shift-left 1 (+ n 3)) 3)
               :else "TOO BIG")))))
