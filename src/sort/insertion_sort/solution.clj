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
    (let [toks (.split (.trim s) "\\s+")
          nt (alength toks)
          vals (long-array nt)]
      (loop [i 0]
        (when (< i nt)
          (aset vals i (Long/parseLong (aget toks i)))
          (recur (inc i))))
      (let [n (aget vals 0)
            k (max 0 (min n (dec nt)))
            arr (long-array k)]
        (System/arraycopy vals 1 arr 0 (int k))
        (insertion-sort arr)
        (let [sb (StringBuilder. "[")]
          (loop [i 0]
            (when (< i k)
              (when (pos? i) (.append sb ","))
              (.append sb (aget arr i))
              (recur (inc i))))
          (.append sb "]")
          (print (.toString sb)))))))
