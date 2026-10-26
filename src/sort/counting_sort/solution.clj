;; Counting sort: read n then n integers, print them sorted as [a,b,c].
;; The count array is offset by the minimum value, so negative inputs
;; work too; its size is max - min + 1.
(let [tokens (vec (.split (.trim (slurp *in*)) "\\s+"))]
  (when (and (seq tokens) (not= [""] tokens))
    (let [nums (mapv #(Long/parseLong %) tokens)
          n (first nums)
          a (vec (take n (rest nums)))
          sb (StringBuilder. "[")]
      (when (seq a)
        (let [mn (reduce min a)
              mx (reduce max a)
              cnt (long-array (inc (- mx mn)))]
          (doseq [x a]
            (aset cnt (- x mn) (inc (aget cnt (- x mn)))))
          (loop [v mn, printed 0]
            (when (<= v mx)
              (let [c (aget cnt (- v mn))]
                (dotimes [i c]
                  (when (pos? (+ printed i))
                    (.append sb ","))
                  (.append sb (str v)))
                (recur (inc v) (+ printed c)))))))
      (.append sb "]")
      (print (str sb)))))
