;; Multiprocessor makespan: list scheduling + LPT rule (CLRS 35).
(require '[clojure.string :as str])

(defn- ms-schedule
  [m jobs]
  (loop [heap (loop [i 0 h (sorted-set)]
                (if (>= i m) h (recur (inc i) (conj h [0 i]))))
         js jobs best 0]
    (if (empty? js)
      best
      (let [[load i] (first heap)
            nl (+ load (first js))]
        (recur (conj (disj heap [load i]) [nl i]) (rest js) (max best nl))))))

(let [s (slurp *in*)]
  (when-not (str/blank? s)
    (let [toks (mapv #(Long/parseLong %) (str/split (str/trim s) #"\s+"))
          m (nth toks 0)
          n (nth toks 1)
          jobs (subvec toks 2 (+ 2 n))
          first (ms-schedule m jobs)
          lpt (ms-schedule m (vec (reverse (sort jobs))))]
      (print (str first "\n" lpt)))))
