;; Multiprocessor makespan: list scheduling + LPT rule (CLRS 35).
(require '[clojure.string :as str])
(import '[java.util PriorityQueue Comparator])

(defn- ms-schedule
  [m jobs]
  (let [cmp (reify Comparator
              (compare [_ a b]
                (let [c (compare (nth a 0) (nth b 0))]
                  (if (zero? c) (compare (nth a 1) (nth b 1)) c))))
        heap (PriorityQueue. 16 cmp)]
    (doseq [i (range m)]
      (.add heap [0 i]))
    (loop [js jobs best 0]
      (if (empty? js)
        best
        (let [[load i] (.poll heap)
              nl (+ load (first js))]
          (.add heap [nl i])
          (recur (rest js) (max best nl)))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          m (nth toks 0)
          n (nth toks 1)
          jobs (subvec toks 2 (+ 2 n))
          first (ms-schedule m jobs)
          lpt (ms-schedule m (vec (reverse (sort jobs))))]
      (print (str first "\n" lpt)))))
