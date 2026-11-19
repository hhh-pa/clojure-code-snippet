;; K-way merge of sorted lists (heap, O(N log k)).
(require '[clojure.string :as str])

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          k (nth toks 0)
          [lists _] (loop [i 0 p 1 acc []]
                      (if (>= i k)
                        [acc p]
                        (let [m (nth toks p)]
                          (recur (inc i) (+ p 1 m) (conj acc (subvec toks (+ p 1) (+ p 1 m)))))))
          heap (loop [i 0 h (sorted-set)]
                 (if (>= i k)
                   h
                   (let [lst (nth lists i)]
                     (recur (inc i) (if (seq lst) (conj h [(nth lst 0) i 0]) h)))))]
      (loop [heap heap out []]
        (if (empty? heap)
          (print (str/join " " out))
          (let [[v i j] (first heap)
                lst (nth lists i)
                nj (inc j)]
            (recur (if (< nj (count lst))
                     (conj (disj heap [v i j]) [(nth lst nj) i nj])
                     (disj heap [v i j]))
                   (conj out v))))))))
