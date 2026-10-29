(require '[clojure.string :as str])

(defn tournament-sort [^longs a]
  (let [n (alength a)]
    (when (> n 1)
      (let [size (loop [s 1] (if (< s n) (recur (* s 2)) s))
            ^longs tree (long-array (* 2 size))]
        (loop [i 0]
          (when (< i size)
            (aset tree (+ size i) (if (< i n) i -1))
            (recur (inc i))))
        (let [val (fn [idx] (if (neg? idx) Long/MAX_VALUE (aget a idx)))
              winner (fn [x y] (if (<= (val x) (val y)) x y))]
          (loop [i (dec size)]
            (when (>= i 1)
              (aset tree i (long (winner (aget tree (* 2 i)) (aget tree (inc (* 2 i))))))
              (recur (dec i))))
          (let [^longs out (long-array n)]
            (loop [k 0]
              (when (< k n)
                (let [w (aget tree 1)]
                  (aset out k (aget a w))
                  (aset tree (+ size w) -1)
                  (loop [i (quot (+ size w) 2)]
                    (when (>= i 1)
                      (aset tree i (long (winner (aget tree (* 2 i)) (aget tree (inc (* 2 i))))))
                      (recur (quot i 2))))
                  (recur (inc k)))))
            (System/arraycopy out 0 a 0 n)))))))

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
        (tournament-sort arr)
        (let [sb (StringBuilder. "[")]
          (loop [i 0]
            (when (< i k)
              (when (pos? i) (.append sb ","))
              (.append sb (aget arr i))
              (recur (inc i))))
          (.append sb "]")
          (print (.toString sb)))))))
