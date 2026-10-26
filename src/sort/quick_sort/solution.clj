(require '[clojure.string :as str])

(defn quick-sort [^longs a]
  (let [n (alength a)
        swap (fn [i j]
               (let [t (aget a i)]
                 (aset a i (aget a j))
                 (aset a j t)))]
    (letfn [(part [lo hi]
              (let [pivot (aget a hi)]
                (loop [i lo j lo]
                  (if (< j hi)
                    (if (<= (aget a j) pivot)
                      (do (swap i j) (recur (inc i) (inc j)))
                      (recur i (inc j)))
                    (do (swap i hi) i)))))
            (qsort [lo hi]
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
    (let [toks (vec (map #(Long/parseLong %) (remove #(= % "") (str/split s #"\s+"))))
          n (first toks)
          arr (long-array (take n (rest toks)))]
      (quick-sort arr)
      (print (str "[" (str/join "," (vec arr)) "]")))))
