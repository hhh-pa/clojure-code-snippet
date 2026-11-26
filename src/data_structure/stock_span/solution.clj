;; Stock span problem (monotonic stack, O(n)).
(require '[clojure.string :as str])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          n (nth toks 0)
          p (subvec toks 1 (inc n))]
      (loop [i 0 stack [] out []]
        (if (>= i n)
          (print (str/join " " out))
          (let [x (nth p i)]
            (if (and (seq stack) (<= (nth p (peek stack)) x))
              (recur i (pop stack) out)
              (recur (inc i) (conj stack i)
                     (conj out (if (empty? stack) (inc i) (- i (peek stack))))))))))))
