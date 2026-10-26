(require '[clojure.string :as str])

(defn bucket-sort [^longs a]
  (let [n (alength a)]
    (when (pos? n)
      (let [mn (loop [i 1 m (aget a 0)]
                 (if (< i n) (recur (inc i) (min m (aget a i))) m))
            mx (loop [i 1 m (aget a 0)]
                 (if (< i n) (recur (inc i) (max m (aget a i))) m))]
        (when (not= mn mx)
          (let [nb (min n 8)
                buckets (vec (repeatedly nb (fn [] (java.util.ArrayList.))))
                span (inc (- mx mn))]
            (loop [i 0]
              (when (< i n)
                (let [v (aget a i)
                      idx (int (quot (* (- v mn) nb) span))]
                  (.add ^java.util.ArrayList (nth buckets idx) v)
                  (recur (inc i)))))
            (let [k (atom 0)]
              (doseq [b (range nb)]
                (let [^java.util.ArrayList lst (nth buckets b)]
                  (java.util.Collections/sort lst)
                  (dotimes [j (.size lst)]
                    (aset a @k (long (.get lst j)))
                    (swap! k inc)))))))))))

(let [s (slurp *in*)]
  (when (not (str/blank? s))
    (let [toks (vec (map #(Long/parseLong %) (remove #(= % "") (str/split s #"\s+"))))
          n (first toks)
          arr (long-array (take n (rest toks)))]
      (bucket-sort arr)
      (print (str "[" (str/join "," (vec arr)) "]")))))
