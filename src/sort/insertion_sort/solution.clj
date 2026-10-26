(require '[clojure.string :as str])

(defn insertion-sort [^longs a]
  (let [n (alength a)]
    (loop [i 1]
      (when (< i n)
        (let [key (aget a i)]
          (loop [j (dec i)]
            (if (and (>= j 0) (> (aget a j) key))
              (do (aset a (inc j) (aget a j))
                  (recur (dec j)))
              (aset a (inc j) key))))
        (recur (inc i))))))

(let [s (slurp *in*)]
  (when (not (str/blank? s))
    (let [toks (vec (map #(Long/parseLong %) (remove #(= % "") (str/split s #"\s+"))))
          n (first toks)
          arr (long-array (take n (rest toks)))]
      (insertion-sort arr)
      (print (str "[" (str/join "," (vec arr)) "]")))))
