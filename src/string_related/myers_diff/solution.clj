;; Myers O(ND) shortest edit script (insert/delete only).

(let [toks (vec (.split (.trim (slurp *in*)) " " -1))
      a (nth toks 1 "") b (nth toks 2 "")
      n (count a) m (count b)]
  (loop [d 0 v {1 0} trace []]
    (let [[cur found] (loop [k (- d) cur {}]
                        (if (> k d) [cur nil]
                            (let [x (if (or (= k (- d)) (and (not= k d) (< (get v (dec k) -1) (get v (inc k) -1))))
                                        (get v (inc k) 0)
                                        (inc (get v (dec k) 0)))
                                  y (- x k)
                                  [x y] (loop [x x y y]
                                            (if (and (< x n) (< y m) (= (nth a x) (nth b y)))
                                              (recur (inc x) (inc y))
                                              [x y]))]
                              (if (and (>= x n) (>= y m))
                                [(assoc cur k x) d]
                                (recur (+ k 2) (assoc cur k x))))))]
      (if found
        (let [trace (conj trace v)
              ops (loop [d found x n y m acc []]
                      (if (zero? d)
                        (loop [x x y y acc acc]
                          (if (and (pos? x) (pos? y)) (recur (dec x) (dec y) (conj acc \=)) acc))
                        (let [vv (nth trace d) k (- x y)]
                          (if (or (= k (- d)) (and (not= k d) (< (get vv (dec k) -1) (get vv (inc k) -1))))
                            (let [px (get vv (inc k))]
                              (recur (dec d) px (- px (inc k))
                                     (into acc (concat (repeat (- x px) \=) [\+]))))
                            (let [px (get vv (dec k))]
                              (recur (dec d) px (- px (dec k))
                                     (into acc (concat (repeat (- x (inc px)) \=) [\-]))))))))]
          (print (str found "\n" (apply str (reverse ops)))))
        (recur (inc d) cur (conj trace v))))))
