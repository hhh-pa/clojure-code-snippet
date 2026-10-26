(require '[clojure.string :as str])
(defn edit-dist [^String a ^String b]
  (let [m (.length a)
        n (.length b)
        ac (.toCharArray a)
        bc (.toCharArray b)
        prev (int-array (inc m))
        curr (int-array (inc m))]
    (dotimes [i (inc m)] (aset ^ints prev i (int i)))
    (loop [i 1 ^ints p prev ^ints c curr]
      (if (> i n)
        (aget p m)
        (do
          (aset c 0 (int i))
          (let [bi (aget ^chars bc (dec i))]
            (loop [j 1]
              (when (<= j m)
                (if (zero? (Character/compare (aget ^chars ac (dec j)) bi))
                  (aset c j (aget p (dec j)))
                  (let [x (aget p j)
                        y (aget c (dec j))
                        z (aget p (dec j))
                        q (if (< x y) x y)]
                    (aset c j (int (inc (if (< q z) q z))))))
                (recur (inc j)))))
          (recur (inc i) c p))))))
(let [data (slurp *in*)]
  (when-not (str/blank? data)
    (let [toks (.split (.trim ^String data) "\\s+")
          t (Long/parseLong (aget ^objects toks 0))
          sb (StringBuilder.)]
      (loop [k (long 0) idx 1]
        (when (< k t)
          (let [a (aget ^objects toks idx)
                b (aget ^objects toks (inc idx))]
            (.append sb (edit-dist a b))
            (.append sb "\n"))
          (recur (inc k) (+ idx 2))))
      (print (str sb)))))
