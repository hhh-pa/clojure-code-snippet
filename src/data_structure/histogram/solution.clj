;; https://www.hackerrank.com/challenges/largest-rectangle/problem
;; Largest rectangle in a histogram (monotonic stack, O(n)).
(require '[clojure.string :as str])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          n (nth toks 0)
          h (conj (subvec toks 1 (inc n)) 0)]
      (loop [i 0 stack [] best 0]
        (if (> i n)
          (print best)
          (let [x (nth h i)]
            (if (and (seq stack) (> (nth h (peek stack)) x))
              (let [height (nth h (peek stack))
                    rest (pop stack)
                    width (if (empty? rest) i (- i (peek rest) 1))]
                (recur i rest (max best (* height width))))
              (recur (inc i) (conj stack i) best))))))))
