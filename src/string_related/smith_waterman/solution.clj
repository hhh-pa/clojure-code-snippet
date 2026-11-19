;; Smith-Waterman local sequence alignment (general scoring).

(let [toks (vec (.split (.trim (slurp *in*)) " " -1))
      match (Long/parseLong (nth toks 1)) mismatch (Long/parseLong (nth toks 2))
      gap (Long/parseLong (nth toks 3))
      s1 (nth toks 4) s2 (nth toks 5)
      n (count s1) m (count s2)
      f (into-array (map long-array (repeat (inc n) (repeat (inc m) 0))))]
  (loop [i 1 j 1 best 0 bi 0 bj 0]
    (if (> i n)
      (loop [i bi j bj a1 [] a2 []]
        (if (or (zero? i) (zero? j) (zero? (aget ^longs (aget ^objects f i) j)))
          (let [r1 (apply str (reverse a1)) r2 (apply str (reverse a2))]
            (print (if (and (empty? r1) (empty? r2)) (str best) (str best "\n" r1 "\n" r2))))
          (let [sub (if (= (nth s1 (dec i)) (nth s2 (dec j))) match mismatch)
                fi (aget ^objects f i)]
            (cond (= (aget ^longs fi j) (+ (aget ^longs (aget ^objects f (dec i)) (dec j)) sub))
                  (recur (dec i) (dec j) (conj a1 (nth s1 (dec i))) (conj a2 (nth s2 (dec j))))
                  (= (aget ^longs fi j) (+ (aget ^longs (aget ^objects f (dec i)) j) gap))
                  (recur (dec i) j (conj a1 (nth s1 (dec i))) (conj a2 \-))
                  (= (aget ^longs fi j) (+ (aget ^longs (aget ^objects f i) (dec j)) gap))
                  (recur i (dec j) (conj a1 \-) (conj a2 (nth s2 (dec j))))
                  :else (recur 0 0 a1 a2)))))
      (let [sub (if (= (nth s1 (dec i)) (nth s2 (dec j))) match mismatch)
            v (max 0 (+ (aget ^longs (aget ^objects f (dec i)) (dec j)) sub)
                       (+ (aget ^longs (aget ^objects f (dec i)) j) gap)
                       (+ (aget ^longs (aget ^objects f i) (dec j)) gap))]
        (aset ^longs (aget ^objects f i) j v)
        (let [[nb nbi nbj] (if (> v best) [v i j] [best bi bj])]
          (if (>= j m) (recur (inc i) 1 nb nbi nbj) (recur i (inc j) nb nbi nbj)))))))
