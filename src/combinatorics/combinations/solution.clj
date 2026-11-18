;; Lexicographic r-subsets of {1..n} (Brualdi Theorem 4.4.1).
(require '[clojure.string :as str])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          n (nth toks 0) r (nth toks 1)
          last (vec (range (inc (- n r)) (inc n)))]
      (loop [cur (vec (range 1 (inc r))) out []]
        (let [out (conj out (str/join " " cur))]
          (if (= cur last)
            (print (str (count out) "\n" (str/join "\n" out)))
            (let [present (set cur)
                  k (loop [k (dec r)]
                      (if (and (<= (inc (nth cur k)) n) (not (present (inc (nth cur k)))))
                        k
                        (recur (dec k))))
                  nxt (vec (concat (subvec cur 0 k)
                                   (range (inc (nth cur k)) (+ (inc (nth cur k)) (- r k)))))]
              (recur nxt out))))))))
