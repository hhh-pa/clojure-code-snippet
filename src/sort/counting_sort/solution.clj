;; Counting sort: read n then n integers, print them sorted as [a,b,c].
;; The count array is offset by the minimum value, so negative inputs
;; work too; its size is max - min + 1.
(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          nt (alength toks)
          vals (long-array nt)]
      ;; Parse every token up front, like the original mapv-based driver.
      (loop [i 0]
        (when (< i nt)
          (aset vals i (Long/parseLong (aget toks i)))
          (recur (inc i))))
      (let [n (aget vals 0)
            k (max 0 (min n (dec nt)))
            sb (StringBuilder. "[")]
        (when (pos? k)
          ;; Min/max over the used values in one pass each.
          (let [mn (loop [i 2, acc (aget vals 1)]
                     (if (> i k)
                       acc
                       (recur (inc i) (min acc (aget vals i)))))
                mx (loop [i 2, acc (aget vals 1)]
                     (if (> i k)
                       acc
                       (recur (inc i) (max acc (aget vals i)))))
                cnt (long-array (inc (- mx mn)))]
            (loop [i 0]
              (when (< i k)
                (let [idx (- (aget vals (inc i)) mn)]
                  (aset cnt idx (inc (aget cnt idx))))
                (recur (inc i))))
            ;; Emit each value cnt times; append(long) avoids a String
            ;; allocation per element that (str v) used to cause.
            (loop [v mn, first true]
              (when (<= v mx)
                (let [c (aget cnt (- v mn))
                      first (loop [j 0, f first]
                              (if (>= j c)
                                f
                                (do (when-not f (.append sb ","))
                                    (.append sb (long v))
                                    (recur (inc j) false))))]
                  (recur (inc v) first))))))
        (.append sb "]")
        (print (.toString sb))))))
