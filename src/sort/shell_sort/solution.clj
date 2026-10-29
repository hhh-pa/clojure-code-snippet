(require '[clojure.string :as str])

(defn shell-sort [^longs a]
  (let [n (alength a)]
    (loop [gap (quot n 2)]
      (when (pos? gap)
        (loop [i gap]
          (when (< i n)
            (let [tmp (aget a i)]
              (loop [j i]
                (if (and (>= j gap) (> (aget a (- j gap)) tmp))
                  (do (aset a j (aget a (- j gap)))
                      (recur (- j gap)))
                  (aset a j tmp))))
            (recur (inc i))))
        (recur (quot gap 2))))))

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
        (shell-sort arr)
        (let [sb (StringBuilder. "[")]
          (loop [i 0]
            (when (< i k)
              (when (pos? i) (.append sb ","))
              (.append sb (aget arr i))
              (recur (inc i))))
          (.append sb "]")
          (print (.toString sb)))))))
