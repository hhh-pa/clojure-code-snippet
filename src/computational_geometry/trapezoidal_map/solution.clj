;; trapezoidal-map: randomized incremental trapezoidal map with a
;; history DAG for planar point location. Segments insert in
;; fixed-seed shuffled order; each new segment splits the trapezoids
;; it crosses (upper/lower plus end pieces) with X/Y DAG nodes above.
;; Queries walk the DAG; answers are top/bottom segment ids.
(def digit-values (zipmap "0123456789" (range)))
(def separators #{\space \tab \newline \return \formfeed \u000B})
(defn tokenize [s]
  (let [done (reduce (fn [{:keys [tokens current]} ch]
                       (if (contains? separators ch)
                         (if (empty? current) {:tokens tokens :current []}
                             {:tokens (conj tokens (apply str current)) :current []})
                         {:tokens tokens :current (conj current ch)}))
                     {:tokens [] :current []} s)]
    (if (empty? (:current done)) (:tokens done)
        (conj (:tokens done) (apply str (:current done))))))
(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))
(defn parse-token [tok]
  (let [chs (seq tok)]
    (when (seq chs)
      (let [head (first chs) neg? (= head \-) rest-chs (if (or neg? (= head \+)) (rest chs) chs)]
        (when (seq rest-chs)
          (if neg?
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (>= acc (quot (+ min-long d) 10))) (- (* acc 10) d))))) 0 rest-chs)
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs)))))))
(defn cross [a b c]
  (- (* (- (nth b 0) (nth a 0)) (- (nth c 1) (nth a 1)))
     (* (- (nth b 1) (nth a 1)) (- (nth c 0) (nth a 0)))))
(def LCG-A 6364136223846793005)
(def LCG-C 1442695040888963407)
(defn -main []
  (let [nums (mapv parse-token (tokenize (slurp *in*)))
        s (nth nums 0)
        segs (vec (for [i (range s)]
                    (let [x1 (nth nums (+ 1 (* 4 i))) y1 (nth nums (+ 2 (* 4 i)))
                          x2 (nth nums (+ 3 (* 4 i))) y2 (nth nums (+ 4 (* 4 i)))]
                      (if (> x1 x2) [[x2 y2] [x1 y1]] [[x1 y1] [x2 y2]]))))
        ord (loop [i (dec s) st 192 ord (vec (range s))]
              (if (<= i 0) ord
                (let [st2 (unchecked-add (unchecked-multiply st LCG-A) LCG-C)
                      j (mod (unsigned-bit-shift-right st2 33) (inc i))]
                  (recur (dec i) st2 (assoc ord i (nth ord j) j (nth ord i))))))
        X0 (dec (apply min (mapcat (fn [[[ax _] [bx _]]] [ax bx]) segs)))
        X1 (inc (apply max (mapcat (fn [[[ax _] [bx _]]] [ax bx]) segs)))
        above? (fn [si p] (let [[[ax ay] [bx by]] (nth segs si)]
                             (if (= ax bx) false (pos? (cross [ax ay] [bx by] p)))))
        [traps nodes] (loop [oi 0
                             traps [[-1 -1 [X0 0] [X1 0]]]
                             nodes [[0 0 [0 0] -1 -1 -1]]]
                        (if (>= oi s) [traps nodes]
                          (let [si (nth ord oi)
                                p (nth (nth segs si) 0) q (nth (nth segs si) 1)
                                leaves (filterv #(zero? (nth (nth nodes %) 0)) (range (count nodes)))
                                hits (filterv (fn [ni]
                                                (let [[top bot lp rp] (nth traps (nth (nth nodes ni) 1))]
                                                  (if (or (<= (nth rp 0) (nth p 0)) (>= (nth lp 0) (nth q 0))) false
                                                    (let [xm (/ (+ (max (nth lp 0) (nth p 0)) (min (nth rp 0) (nth q 0))) 2.0)
                                                          tt (/ (- xm (nth p 0)) (- (nth q 0) (nth p 0)))
                                                          ys (+ (nth p 1) (* tt (- (nth q 1) (nth p 1))))
                                                          yt (if (neg? top) 4e18
                                                               (let [[[ax ay] [bx by]] (nth segs top)
                                                                     t2 (/ (- xm ax) (- bx ax))]
                                                                 (+ ay (* t2 (- by ay)))))
                                                          yb (if (neg? bot) -4e18
                                                               (let [[[ax ay] [bx by]] (nth segs bot)
                                                                     t2 (/ (- xm ax) (- bx ax))]
                                                                 (+ ay (* t2 (- by ay)))))]
                                                      (and (< yb ys) (< ys yt)))))) leaves)
                                shits (vec (sort-by #(nth (nth traps (nth (nth nodes %) 1)) 2) hits))
                                [traps2 nodes2] (loop [hi 0 traps traps nodes nodes]
                                                  (if (>= hi (count shits)) [traps nodes]
                                                    (let [ni (nth shits hi)
                                                          [top bot lp rp] (nth traps (nth (nth nodes ni) 1))
                                                          fst (zero? hi) lst (= hi (dec (count shits)))
                                                          ulp (if (< (nth lp 0) (nth p 0)) p lp)
                                                          urp (if (> (nth rp 0) (nth q 0)) q rp)
                                                          upT (count traps) traps-a (conj traps [top si ulp urp])
                                                          loT (count traps-a) traps-b (conj traps-a [si bot ulp urp])
                                                          upN (count nodes) nodes-a (conj nodes [0 upT [0 0] -1 -1 -1])
                                                          loN (count nodes-a) nodes-b (conj nodes-a [0 loT [0 0] -1 -1 -1])
                                                          yN (count nodes-b) nodes-c (conj nodes-b [2 -1 [0 0] si upN loN])
                                                          [traps-c nodes-c2 rep] (if (and fst (< (nth lp 0) (nth p 0)))
                                                                                 (let [lT (count traps-b) traps-d (conj traps-b [top bot lp p])
                                                                                       lN (count nodes-c) nodes-d (conj nodes-c [0 lT [0 0] -1 -1 -1])
                                                                                       xN (count nodes-d) nodes-e (conj nodes-d [1 -1 p -1 lN yN])]
                                                                                   [traps-d nodes-e xN])
                                                                                 [traps-b nodes-c yN])
                                                          [traps-d2 nodes-d2 rep2] (if (and lst (> (nth rp 0) (nth q 0)))
                                                                                   (let [rT (count traps-c) traps-e (conj traps-c [top bot q rp])
                                                                                         rN (count nodes-c2) nodes-f (conj nodes-c2 [0 rT [0 0] -1 -1 -1])
                                                                                         xN (count nodes-f) nodes-g (conj nodes-f [1 -1 q -1 rep rN])]
                                                                                     [traps-e nodes-g xN])
                                                                                   [traps-c nodes-c2 rep])]
                                                      (recur (inc hi) traps-d2 (assoc nodes-d2 ni [3 (nth (nth nodes-d2 ni) 1)
                                                                                                   (nth (nth nodes-d2 ni) 2)
                                                                                                   (nth (nth nodes-d2 ni) 3) rep2 -1])))))]
                            (recur (inc oi) traps2 nodes2))))
        base (+ 1 (* 4 s))
        qn (nth nums base)]
    (loop [i 0]
      (when (< i qn)
        (let [gx (nth nums (+ base 1 (* 2 i))) gy (nth nums (+ base 2 (* 2 i)))
              ni (loop [ni 0]
                     (let [[kind _ pt seg left right] (nth nodes ni)]
                       (cond (zero? kind) ni
                             (= kind 3) (recur left)
                             (= kind 1) (recur (if (< gx (nth pt 0)) left right))
                             :else (recur (if (above? seg [gx gy]) left right)))))
              [top bot _ _] (nth traps (nth (nth nodes ni) 1))]
          (println top bot))
        (recur (inc i))))))
(-main)
