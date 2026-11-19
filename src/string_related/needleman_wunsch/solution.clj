;; Needleman-Wunsch global sequence alignment (general scoring).

(let [toks (vec (.split (.trim (slurp *in*)) " " -1))
      match (Long/parseLong (nth toks 1)) mismatch (Long/parseLong (nth toks 2))
      gap (Long/parseLong (nth toks 3))
      s1 (nth toks 4 "") s2 (nth toks 5 "")
      n (count s1) m (count s2)
      f (into-array (map long-array
                          (for [i (range (inc n))]
                            (for [j (range (inc m))]
                              (cond (> i 0) (if (> j 0) 0 (* i gap))
                                    (> j 0) (* j gap)
                                    :else 0)))))]
  (doseq [i (range 1 (inc n)) j (range 1 (inc m))]
    (let [sub (if (= (nth s1 (dec i)) (nth s2 (dec j))) match mismatch)]
      (aset ^longs (aget ^objects f i) j
            (max (+ (aget ^longs (aget ^objects f (dec i)) (dec j)) sub)
                 (+ (aget ^longs (aget ^objects f (dec i)) j) gap)
                 (+ (aget ^longs (aget ^objects f i) (dec j)) gap)))))
  (loop [i n j m a1 [] a2 []]
    (if (and (zero? i) (zero? j))
      (print (str (aget ^longs (aget ^objects f n) m) "\n" (apply str (reverse a1)) "\n" (apply str (reverse a2))))
      (let [fi (aget ^objects f i)]
        (if (and (> i 0) (> j 0)
                 (let [sub (if (= (nth s1 (dec i)) (nth s2 (dec j))) match mismatch)]
                   (= (aget ^longs fi j) (+ (aget ^longs (aget ^objects f (dec i)) (dec j)) sub))))
          (recur (dec i) (dec j) (conj a1 (nth s1 (dec i))) (conj a2 (nth s2 (dec j))))
          (if (and (> i 0) (= (aget ^longs fi j) (+ (aget ^longs (aget ^objects f (dec i)) j) gap)))
            (recur (dec i) j (conj a1 (nth s1 (dec i))) (conj a2 \-))
            (recur i (dec j) (conj a1 \-) (conj a2 (nth s2 (dec j))))))))))
