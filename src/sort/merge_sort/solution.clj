(require '[clojure.string :as str])

(defn merge-sort [^longs a]
  (let [n (alength a)
        aux (long-array n)]
    (letfn [(msort [lo hi]
              (when (> (- hi lo) 1)
                (let [mid (quot (+ lo hi) 2)]
                  (msort lo mid)
                  (msort mid hi)
                  (System/arraycopy a lo aux lo (- hi lo))
                  (loop [i lo j mid k lo]
                    (when (< k hi)
                      (cond
                        (>= j hi)
                        (do (aset a k (aget aux i)) (recur (inc i) j (inc k)))
                        (>= i mid)
                        (do (aset a k (aget aux j)) (recur i (inc j) (inc k)))
                        (<= (aget aux i) (aget aux j))
                        (do (aset a k (aget aux i)) (recur (inc i) j (inc k)))
                        :else
                        (do (aset a k (aget aux j)) (recur i (inc j) (inc k)))))))))]
      (msort 0 n))))

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
        (merge-sort arr)
        (let [sb (StringBuilder. "[")]
          (loop [i 0]
            (when (< i k)
              (when (pos? i) (.append sb ","))
              (.append sb (aget arr i))
              (recur (inc i))))
          (.append sb "]")
          (print (.toString sb)))))))
