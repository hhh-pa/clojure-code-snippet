(require '[clojure.string :as str])

(defn tournament-sort [^longs a]
  (let [n (alength a)]
    (when (> n 1)
      (let [size (loop [s 1] (if (< s n) (recur (* s 2)) s))
            tree (long-array (* 2 size))]
        (loop [i 0]
          (when (< i size)
            (aset tree (+ size i) (if (< i n) i -1))
            (recur (inc i))))
        (let [val (fn [idx] (if (neg? idx) Long/MAX_VALUE (aget a idx)))
              winner (fn [x y] (if (<= (val x) (val y)) x y))]
          (loop [i (dec size)]
            (when (>= i 1)
              (aset tree i (winner (aget tree (* 2 i)) (aget tree (inc (* 2 i)))))
              (recur (dec i))))
          (let [out (long-array n)]
            (loop [k 0]
              (when (< k n)
                (let [w (aget tree 1)]
                  (aset out k (aget a w))
                  (aset tree (+ size w) -1)
                  (loop [i (quot (+ size w) 2)]
                    (when (>= i 1)
                      (aset tree i (winner (aget tree (* 2 i)) (aget tree (inc (* 2 i)))))
                      (recur (quot i 2))))
                  (recur (inc k)))))
            (System/arraycopy out 0 a 0 n)))))))

(let [s (slurp *in*)]
  (when (not (str/blank? s))
    (let [toks (vec (map #(Long/parseLong %) (remove #(= % "") (str/split s #"\s+"))))
          n (first toks)
          arr (long-array (take n (rest toks)))]
      (tournament-sort arr)
      (print (str "[" (str/join "," (vec arr)) "]")))))
