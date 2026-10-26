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
    (let [toks (vec (map #(Long/parseLong %) (remove #(= % "") (str/split s #"\s+"))))
          n (first toks)
          arr (long-array (take n (rest toks)))]
      (heap-sort arr)
      (print (str "[" (str/join "," (vec arr)) "]")))))
