;; Binary-tree codec: preorder-with-nulls in, trimmed level-order out.
(require '[clojure.string :as str])

(defn- ts-build
  "Returns [node next-pos]; node is nil or [v left right]."
  [toks pos]
  (if (= (nth toks pos) "#")
    [nil (inc pos)]
    (let [v (Long/parseLong (nth toks pos))
          [l p1] (ts-build toks (inc pos))
          [r p2] (ts-build toks p1)]
      [[v l r] p2])))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (str/split (str/trim s) #"\s+")
          n (Long/parseLong (first toks))
          pre (vec (take n (rest toks)))
          [root _] (ts-build pre 0)
          out (loop [q (conj clojure.lang.PersistentQueue/EMPTY root) acc []]
                (if (empty? q)
                  acc
                  (let [u (peek q)]
                    (if (nil? u)
                      (recur (pop q) (conj acc "#"))
                      (recur (conj (pop q) (nth u 1) (nth u 2))
                             (conj acc (str (nth u 0))))))))
          out (vec (reverse (drop-while #(= % "#") (reverse out))))]
      (print (str/join " " out)))))
