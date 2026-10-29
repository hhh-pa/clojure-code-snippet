(require '[clojure.string :as str])

(defn radix-sort [^longs a]
  (let [n (alength a)]
    (when (pos? n)
      (let [b (long-array n)
            ^ints cnt (int-array 256)]
        (loop [i 0]
          (when (< i n)
            (aset a i (bit-xor (aget a i) Long/MIN_VALUE))
            (recur (inc i))))
        (loop [shift 0 from a to b]
          (when (< shift 64)
            (java.util.Arrays/fill cnt 0)
            (loop [i 0]
              (when (< i n)
                (let [d (int (bit-and (bit-shift-right (aget from i) shift) 255))]
                  (aset cnt d (inc (aget cnt d))))
                (recur (inc i))))
            (loop [d 0 s 0]
              (when (< d 256)
                (let [c (aget cnt d)]
                  (aset cnt d s)
                  (recur (inc d) (+ s c)))))
            (loop [i 0]
              (when (< i n)
                (let [v (aget from i)
                      d (int (bit-and (bit-shift-right v shift) 255))
                      p (aget cnt d)]
                  (aset to p v)
                  (aset cnt d (inc p)))
                (recur (inc i))))
            (recur (+ shift 8) to from)))
        (loop [i 0]
          (when (< i n)
            (aset a i (bit-xor (aget a i) Long/MIN_VALUE))
            (recur (inc i))))))))

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
        (radix-sort arr)
        (let [sb (StringBuilder. "[")]
          (loop [i 0]
            (when (< i k)
              (when (pos? i) (.append sb ","))
              (.append sb (aget arr i))
              (recur (inc i))))
          (.append sb "]")
          (print (.toString sb)))))))
