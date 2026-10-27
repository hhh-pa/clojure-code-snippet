(require '[clojure.string :as str])

(defn quick-sort [^longs a]
  (let [n (alength a)]
    (letfn [(part [^long lo ^long hi]
              (let [pivot (aget a hi)]
                (loop [i lo j lo]
                  (if (< j hi)
                    (if (<= (aget a j) pivot)
                      (do (let [t (aget a i)]
                            (aset a i (aget a j))
                            (aset a j t))
                          (recur (inc i) (inc j)))
                      (recur i (inc j)))
                    (do (let [t (aget a i)]
                          (aset a i (aget a hi))
                          (aset a hi t))
                        i)))))
            (qsort [^long lo ^long hi]
              (loop [lo lo hi hi]
                (when (< lo hi)
                  (let [p (part lo hi)]
                    (if (< (- p lo) (- hi p))
                      (do (qsort lo (dec p))
                          (recur (inc p) hi))
                      (do (qsort (inc p) hi)
                          (recur lo (dec p))))))))]
      (when (pos? n)
        (qsort 0 (dec n))))))

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
        (quick-sort arr)
        (let [sb (StringBuilder. "[")]
          (loop [i 0]
            (when (< i k)
              (when (pos? i) (.append sb ","))
              (.append sb (aget arr i))
              (recur (inc i))))
          (.append sb "]")
          (print (.toString sb)))))))
