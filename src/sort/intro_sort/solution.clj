(require '[clojure.string :as str])

(defn intro-sort [^longs a]
  (let [n (alength a)
        swap (fn [i j]
               (let [t (aget a i)]
                 (aset a i (aget a j))
                 (aset a j t)))
        depth-limit (if (pos? n)
                      (* 2 (int (/ (Math/log (double n)) (Math/log 2.0))))
                      0)]
    (letfn [(insertion [lo hi]
              (loop [i (inc lo)]
                (when (<= i hi)
                  (let [key (aget a i)]
                    (loop [j (dec i)]
                      (if (and (>= j lo) (> (aget a j) key))
                        (do (aset a (inc j) (aget a j))
                            (recur (dec j)))
                        (aset a (inc j) key))))
                  (recur (inc i)))))
            (sift [lo heap-n root]
              (loop [r root]
                (let [l (inc (* 2 r))]
                  (when (< l heap-n)
                    (let [r2 (inc l)
                          c (if (and (< r2 heap-n)
                                     (> (aget a (+ lo r2)) (aget a (+ lo l))))
                                r2 l)]
                      (when (> (aget a (+ lo c)) (aget a (+ lo r)))
                        (swap (+ lo r) (+ lo c))
                        (recur c)))))))
            (heap-range [lo hi]
              (let [m (inc (- hi lo))]
                (loop [i (quot (- m 2) 2)]
                  (when (>= i 0)
                    (sift lo m i)
                    (recur (dec i))))
                (loop [e (dec m)]
                  (when (pos? e)
                    (swap lo (+ lo e))
                    (sift lo e 0)
                    (recur (dec e))))))
            (part [lo hi]
              (let [pivot (aget a hi)]
                (loop [i lo j lo]
                  (if (< j hi)
                    (if (<= (aget a j) pivot)
                      (do (swap i j) (recur (inc i) (inc j)))
                      (recur i (inc j)))
                    (do (swap i hi) i)))))
            (isort [lo hi depth]
              (loop [lo lo hi hi depth depth]
                (let [size (inc (- hi lo))]
                  (cond
                    (<= size 1) nil
                    (< size 16) (insertion lo hi)
                    (zero? depth) (heap-range lo hi)
                    :else (let [mid (quot (+ lo hi) 2)]
                            (swap mid hi)
                            (let [p (part lo hi)]
                              (isort lo (dec p) (dec depth))
                              (recur (inc p) hi (dec depth))))))))]
      (when (pos? n)
        (isort 0 (dec n) depth-limit)))))

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
        (intro-sort arr)
        (let [sb (StringBuilder. "[")]
          (loop [i 0]
            (when (< i k)
              (when (pos? i) (.append sb ","))
              (.append sb (aget arr i))
              (recur (inc i))))
          (.append sb "]")
          (print (.toString sb)))))))
