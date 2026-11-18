;; Integer bit tricks (exact arithmetic on values < 2^62).
(require '[clojure.string :as str])

(defn- bt-solve
  [op a b]
  (cond
    (= op "POPCOUNT") (Long/bitCount a)
    (= op "PARITY") (bit-and (Long/bitCount a) 1)
    (= op "ISPOW2") (if (and (pos? a) (zero? (bit-and a (dec a)))) 1 0)
    (= op "CTZ") (if (zero? a) 64 (Long/numberOfTrailingZeros a))
    (= op "REVERSE") (loop [i 0 r 0]
                       (if (>= i b)
                         r
                         (recur (inc i)
                                (bit-or r (bit-shift-left (bit-and (bit-shift-right a i) 1)
                                                          (- b 1 i))))))
    :else (if (<= a 1)
            1
            (loop [p 1]
              (if (>= p a) p (recur (bit-shift-left p 1)))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (str/split (str/trim s) #"\s+")
          q (Long/parseLong (first toks))]
      (loop [i 0 p 1 out []]
        (if (>= i q)
          (print (str/join "\n" out))
          (let [op (nth toks p)
                a (Long/parseLong (nth toks (+ p 1)))]
            (if (= op "REVERSE")
              (let [b (Long/parseLong (nth toks (+ p 2)))]
                (recur (inc i) (+ p 3) (conj out (bt-solve op a b))))
              (recur (inc i) (+ p 2) (conj out (bt-solve op a 0))))))))))
