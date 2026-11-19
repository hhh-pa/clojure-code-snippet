;; RC4 stream cipher (historic, broken): KSA + PRGA byte keystream.
(require '[clojure.string :as str])

(defn hex-val [c]
  (Character/digit ^char c 16))

(defn parse-hex [s]
  (mapv (fn [i] (+ (* (hex-val (nth s i)) 16) (hex-val (nth s (inc i)))))
        (range 0 (count s) 2)))

(defn rc4-crypt [key data]
  (let [s (loop [i 0
                 j 0
                 s (vec (range 256))]
            (if (>= i 256)
              s
              (let [j (rem (+ j (nth s i) (nth key (rem i (count key)))) 256)]
                (recur (inc i) j (assoc s i (nth s j) j (nth s i))))))]
    (loop [i 0
           j 0
           s s
           ds (seq data)
           out []]
      (if (empty? ds)
        out
        (let [i (rem (inc i) 256)
              j (rem (+ j (nth s i)) 256)
              s (assoc s i (nth s j) j (nth s i))]
          (recur i j s (rest ds)
                 (conj out (bit-xor (first ds) (nth s (rem (+ (nth s i) (nth s j)) 256))))))))))

(defn -main
  "Read key and plaintext hex; print the ciphertext as hex."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 1)
      (let [key (parse-hex (nth toks 0))
            msg (if (> (count toks) 1) (parse-hex (nth toks 1)) [])]
        (print (apply str (map #(format "%02x" %) (rc4-crypt key msg))))))))

(-main (slurp *in*))
