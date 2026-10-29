(require '[clojure.string :as str])

(defn heap-sort [^longs a]
  (let [n (alength a)
        swap (fn [i j]
               (let [t (aget a i)]
                 (aset a i (aget a j))
                 (aset a j t)))]
    (letfn [(sift [root end]
              (loop [r root]
                (let [l (inc (* 2 r))]
                  (when (<= l end)
                    (let [r2 (inc l)
                          c (if (and (<= r2 end) (> (aget a r2) (aget a l))) r2 l)]
                      (when (> (aget a c) (aget a r))
                        (swap r c)
                        (recur c)))))))]
      (loop [i (quot (- n 2) 2)]
        (when (>= i 0)
          (sift i (dec n))
          (recur (dec i))))
      (loop [e (dec n)]
        (when (pos? e)
          (swap 0 e)
          (sift 0 (dec e))
          (recur (dec e)))))))

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
        (heap-sort arr)
        (let [sb (StringBuilder. "[")]
          (loop [i 0]
            (when (< i k)
              (when (pos? i) (.append sb ","))
              (.append sb (aget arr i))
              (recur (inc i))))
          (.append sb "]")
          (print (.toString sb)))))))
