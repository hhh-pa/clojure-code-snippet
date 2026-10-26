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
    (let [toks (vec (map #(Long/parseLong %) (remove #(= % "") (str/split s #"\s+"))))
          n (first toks)
          arr (long-array (take n (rest toks)))]
      (radix-sort arr)
      (print (str "[" (str/join "," (vec arr)) "]")))))
