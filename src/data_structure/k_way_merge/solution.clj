;; K-way merge of sorted lists (heap, O(N log k)).
(require '[clojure.string :as str])
(import '[java.util PriorityQueue Comparator])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          k (nth toks 0)
          [lists _] (loop [i 0 p 1 acc []]
                      (if (>= i k)
                        [acc p]
                        (let [m (nth toks p)]
                          (recur (inc i) (+ p 1 m) (conj acc (subvec toks (+ p 1) (+ p 1 m)))))))
          cmp (reify Comparator
                (compare [_ a b]
                  (let [c (compare (nth a 0) (nth b 0))]
                    (if (zero? c) (compare (nth a 1) (nth b 1)) c))))
          heap (PriorityQueue. 16 cmp)]
      (doseq [i (range k)]
        (when (seq (nth lists i))
          (.add heap [(nth (nth lists i) 0) i 0])))
      (loop [out []]
        (if (.isEmpty heap)
          (print (str/join " " out))
          (let [[v i j] (.poll heap)
                lst (nth lists i)]
            (when (< (inc j) (count lst))
              (.add heap [(nth lst (inc j)) i (inc j)]))
            (recur (conj out v))))))))
