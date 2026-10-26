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
    (let [toks (vec (map #(Long/parseLong %) (remove #(= % "") (str/split s #"\s+"))))
          n (first toks)
          arr (long-array (take n (rest toks)))]
      (shell-sort arr)
      (print (str "[" (str/join "," (vec arr)) "]")))))
