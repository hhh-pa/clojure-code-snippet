;; Vernam one-time pad: byte-wise XOR of equal-length key and message.
(require '[clojure.string :as str])

(defn hex-val [c]
  (Character/digit ^char c 16))

(defn parse-hex [s]
  (mapv (fn [i] (+ (* (hex-val (nth s i)) 16) (hex-val (nth s (inc i)))))
        (range 0 (count s) 2)))

(defn -main
  "Read key and message hex; print the XOR as hex."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 2)
      (let [key (parse-hex (nth toks 0))
            msg (parse-hex (nth toks 1))]
        (print (apply str (map #(format "%02x" %) (map bit-xor key msg))))))))

(-main (slurp *in*))
