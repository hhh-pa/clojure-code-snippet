;; Sorting unsigned permutations by reversals (first heuristic, 4-approx).

(defn breakpoints [p]
  (let [n (count p)]
    (+ (if (not= (first p) 1) 1 0)
       (if (not= (peek p) n) 1 0)
       (count (filter (fn [[a b]] (not= (abs (- a b)) 1)) (partition 2 1 p))))))

(defn strips [p]
  (let [n (count p)]
    (loop [i 0 l 0 res []]
      (if (>= i (dec n))
        (conj res [l (dec n)])
        (if (not= (abs (- (nth p i) (nth p (inc i)))) 1)
          (recur (inc i) (inc i) (conj res [l i]))
          (recur (inc i) l res))))))

(defn decreasing? [p l r]
  (or (= l r) (every? (fn [j] (> (nth p j) (nth p (inc j)))) (range l r))))

(let [toks (vec (.split (.trim (slurp *in*)) "\\s+"))
      p0 (mapv #(Long/parseLong %) (rest toks))
      n (count p0)]
  (loop [p p0 ops []]
    (if (zero? (breakpoints p))
      (print (str (count ops) (apply str (mapcat (fn [[l r]] [" " (inc l) " " (inc r)]) ops))))
      (let [dec (some (fn [[l r]] (decreasing? p l r)) (strips p))
            cur (breakpoints p)
            found (first (for [l (range n) r (range (inc l) n)
                               :let [q (vec (concat (subvec p 0 l) (reverse (subvec p l (inc r))) (subvec p (inc r))))
                                     bq (breakpoints q)]
                               :when (if dec
                                       (< bq cur)
                                       (and (<= bq cur)
                                            (some (fn [[a b]] (decreasing? q a b)) (strips q))))]
                           [q l r]))]
        (recur (nth found 0) (conj ops [(nth found 1) (nth found 2)]))))))
