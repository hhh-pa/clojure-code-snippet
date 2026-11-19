;; 1D parametric alignment: exact decomposition over gap penalty.
(require '[clojure.string :as str])

(defn gcd [a b]
  (let [a (if (neg? a) (- a) a)
        b (if (neg? b) (- b) b)]
    (loop [a a b b]
      (if (zero? b) a (recur b (rem a b))))))

;; Breakpoint values as exact [num den] integer pairs (no Ratio type:
;; clojure-rs has no proven ratio support, so compare by
;; cross-multiplication and print reduced fractions manually, as in
;; max-density-subgraph).
(defn ixv [a b]
  [(- (nth b 1) (nth a 1)) (- (nth a 0) (nth b 0))])

(defn ixge [a b c]
  (let [[n1 d1] (ixv a b)
        [n2 d2] (ixv b c)
        l (* n1 d2) r (* n2 d1)]
    (if (neg? (* d1 d2)) (<= l r) (>= l r))))

(defn xmaxg [v g]
  (let [[n1 d1] v [n2 d2] g l (* n1 d2) r (* n2 d1)]
    (if (if (neg? (* d1 d2)) (<= l r) (>= l r)) v g)))

(defn xminn [v g]
  (let [[n1 d1] v [n2 d2] g l (* n1 d2) r (* n2 d1)]
    (if (if (neg? (* d1 d2)) (>= l r) (<= l r)) v g)))

(defn xlt [[n1 d1] [n2 d2]]
  (let [l (* n1 d2) r (* n2 d1)]
    (if (neg? (* d1 d2)) (> l r) (< l r))))

(defn xstr [[n d]]
  (let [[n d] (if (neg? d) [(- n) (- d)] [n d])
        g (gcd n d)
        n (quot n g) d (quot d g)]
    (if (= d 1) (str n) (str n "/" d))))

(let [toks (vec (.split (.trim (slurp *in*)) "\\s+"))
      match (Long/parseLong (nth toks 1)) mismatch (Long/parseLong (nth toks 2))
      g0 (Long/parseLong (nth toks 3)) g1 (Long/parseLong (nth toks 4))
      s1 (nth toks 5) s2 (nth toks 6)
      n (count s1) m (count s2) k (+ n m)
      neg -1000000000000000000
      dp0 (vec (repeat (inc n) (vec (repeat (inc m) (vec (repeat (inc k) neg))))))
      par0 (vec (repeat (inc n) (vec (repeat (inc m) (vec (repeat (inc k) 0))))))
      dp (assoc-in dp0 [0 0 0] 0)
      dp (reduce (fn [dp i] (assoc-in dp [i 0 i] 0)) dp (range 1 (inc n)))
      par (reduce (fn [par i] (assoc-in par [i 0 i] 2)) par0 (range 1 (inc n)))
      dp (reduce (fn [dp j] (assoc-in dp [0 j j] 0)) dp (range 1 (inc m)))
      par (reduce (fn [par j] (assoc-in par [0 j j] 3)) par (range 1 (inc m)))
      [dp par] (reduce (fn [[dp par] i]
                         (reduce (fn [[dp par] j]
                                   (reduce (fn [[dp par] t]
                                             (let [sub (if (= (nth s1 (dec i)) (nth s2 (dec j))) match mismatch)
                                                   dv (get-in dp [(dec i) (dec j) t])
                                                     uv (if (pos? t) (get-in dp [(dec i) j (dec t)]) neg)
                                                     lv (if (pos? t) (get-in dp [i (dec j) (dec t)]) neg)
                                                     d (if (= dv neg) neg (+ dv sub))
                                                     cands (filter #(not= (first %) neg) [[d 1] [uv 2] [lv 3]])
                                                     best (if (empty? cands) [neg 0] (reduce (fn [a b] (if (> (first b) (first a)) b a)) cands))]
                                                 [(assoc-in dp [i j t] (first best)) (assoc-in par [i j t] (second best))]))
                                             [dp par] (range (inc k))))
                                 [dp par] (range 1 (inc m))))
                       [dp par] (range 1 (inc n)))
      lines (vec (for [s (range (inc k))
                         :let [c (get-in dp [n m s])]
                         :when (not= c neg)]
                     (loop [i n j m t s mt 0 ms 0]
                       (if (and (zero? i) (zero? j)) [(- s) c mt ms s]
                           (case (get-in par [i j t])
                             1 (recur (dec i) (dec j) t
                                      (if (= (nth s1 (dec i)) (nth s2 (dec j))) (inc mt) mt)
                                      (if (= (nth s1 (dec i)) (nth s2 (dec j))) ms (inc ms)))
                             2 (recur (dec i) j (dec t) mt ms)
                             (recur i (dec j) (dec t) mt ms))))))
      lines (vec (distinct (sort-by #(nth % 4) > lines)))
      hull (reduce (fn [h ln]
                       (loop [h h]
                         (if (and (>= (count h) 2) (ixge (nth h (- (count h) 2)) (peek h) ln))
                           (recur (pop h))
                           (conj h ln))))
                     [] lines)
      segs (vec (for [i (range (count hull))
                        :let [ln (nth hull i)
                              lo (if (zero? i) [g0 1] (ixv (nth hull (dec i)) ln))
                              hi (if (= i (dec (count hull))) [g1 1] (ixv ln (nth hull (inc i))))
                              lo (xmaxg lo [g0 1]) hi (xminn hi [g1 1])]
                        :when (xlt lo hi)]
                    [(xstr lo) (xstr hi) (nth ln 2) (nth ln 3) (nth ln 4)]))
      segs (if (empty? segs)
               (let [best (apply max-key (fn [ln] (+ (nth ln 1) (* (nth ln 0) g0))) hull)]
                 [[(xstr [g0 1]) (xstr [g1 1]) (nth best 2) (nth best 3) (nth best 4)]])
               segs)]
  (print (str/join "\n" (map (fn [[lo hi mt ms s]] (str lo " " hi " " mt " " ms " " s)) segs))))
