;; segment-stabbing: report segments containing query points.
;; Elementary intervals from unique endpoints; each segment lives at
;; the canonical nodes covering its span; a point query collects the
;; sets along its root-leaf path. Matching ids print sorted.
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
(defn st-insert [t v l r ql qr id]
  (cond (or (>= ql r) (<= qr l)) t
        (and (<= ql l) (>= qr r)) (assoc t v (conj (nth t v) id))
        :else (let [m (quot (+ l r) 2)]
                (st-insert (st-insert t (* 2 v) l m ql qr id) (inc (* 2 v)) m r ql qr id))))
(defn bisect-right
  "First index i in sorted vector u with (nth u i) > x; (count u)
   when no entry exceeds x. Binary search: O(log n)."
  [u x]
  (loop [lo 0 hi (count u)]
    (if (>= lo hi)
      lo
      (let [mid (quot (+ lo hi) 2)]
        (if (<= (nth u mid) x)
          (recur (inc mid) hi)
          (recur lo mid))))))
(defn st-query [t v l r i]
  (let [here (nth t v)]
    (if (= (- r l) 1) here
      (let [m (quot (+ l r) 2)]
        (concat here (if (< i m) (st-query t (* 2 v) l m i) (st-query t (inc (* 2 v)) m r i)))))))
(defn -main []
  (let [nums (mapv parse-token (tokenize (slurp *in*)))
        n (nth nums 0)
        segs (vec (for [i (range n)] [(nth nums (+ 1 (* 2 i))) (nth nums (+ 2 (* 2 i)))]))
        u (vec (sort (set (mapcat identity segs))))
        m (count u)
        N (inc (* 2 m))
        rank (zipmap u (range))
        t (reduce (fn [t [i [l r]]] (st-insert t 1 0 N (inc (* 2 (get rank l))) (+ 2 (* 2 (get rank r))) i))
                  (vec (repeat (* 4 N) [])) (map-indexed vector segs))
        base (+ 1 (* 2 n))
        q (nth nums base)]
    (loop [i 0]
      (when (< i q)
        (let [x (nth nums (+ base 1 i))
              ei (cond (< x (nth u 0)) 0
                       (> x (nth u (dec m))) (* 2 m)
                       :else (let [k (dec (bisect-right u x))]
                               (if (= (nth u k) x) (inc (* 2 k)) (+ 2 (* 2 k)))))
              found (vec (sort (st-query t 1 0 N ei)))]
          (println (count found))
          (when (seq found) (println (apply str (interpose " " found)))))
        (recur (inc i))))))
(-main)
