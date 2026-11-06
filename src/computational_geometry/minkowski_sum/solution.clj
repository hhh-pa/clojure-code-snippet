;; minkowski-sum: Minkowski sum of two convex polygons as the
;; convex hull (Andrew monotone chain) of all pairwise vertex sums.
;; Exact integer arithmetic; hull starts at lexicographic minimum.
(def digit-values (zipmap "0123456789" (range)))
(def separators #{\space \tab \newline \return \formfeed \u000B})
(defn tokenize
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))
(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))
(defn parse-token
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))
(defn cross [o a b]
  (- (* (- (nth a 0) (nth o 0)) (- (nth b 1) (nth o 1)))
     (* (- (nth a 1) (nth o 1)) (- (nth b 0) (nth o 0)))))
(defn chain [pts]
  (reduce (fn [h q]
            (loop [h h]
              (if (and (>= (count h) 2)
                       (<= (cross (nth h (- (count h) 2)) (peek h) q) 0))
                (recur (pop h)) (conj h q))))
          [] pts))
(defn -main []
  (let [nums (mapv parse-token (tokenize (slurp *in*)))
        n (nth nums 0)
        a (vec (for [i (range n)] [(nth nums (+ 1 (* 2 i))) (nth nums (+ 2 (* 2 i)))]))
        mbase (+ 1 (* 2 n))
        m (nth nums mbase)
        b (vec (for [i (range m)] [(nth nums (+ mbase 1 (* 2 i))) (nth nums (+ mbase 2 (* 2 i)))]))
        pts (vec (sort (distinct (vec (for [q a r b] [(+ (nth q 0) (nth r 0)) (+ (nth q 1) (nth r 1))])))))
        lo (chain pts)
        hi (chain (vec (reverse pts)))
        hull (vec (concat (pop lo) (pop hi)))]
    (println (count hull))
    (doseq [[x y] hull] (println x y))))
(-main)
