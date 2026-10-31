;; spanning-tree-protocol: 802.1D root election + ports (Tanenbaum).
;; Input: B E; E lines u v cost (bridge IDs = priority). Root = lowest
;; ID. Output: root; per bridge rootport (neighbor/-1); per link
;; designated bridge. Pure Clojure.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize [s]
  (let [done (reduce (fn [{:keys [tokens current]} ch]
                       (if (contains? separators ch)
                         (if (empty? current)
                           {:tokens tokens :current []}
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
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)]
              (when (and (some? d) (>= acc (quot (+ min-long d) 10))) (- (* acc 10) d))))) 0 rest-chs)
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)]
              (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs)))))))

(defn read-int [tok] (parse-token tok))

(def INFLL 4000000000000000000)

(defn solve [toks]
  (let [B (read-int (nth toks 0)) E (read-int (nth toks 1))
        [links adj] (loop [i 0 ls [] ad (vec (repeat B []))]
                      (if (>= i E) [ls ad]
                        (let [u (read-int (nth toks (+ 2 (* i 3))))
                              v (read-int (nth toks (+ 3 (* i 3))))
                              c (read-int (nth toks (+ 4 (* i 3))))]
                          (recur (+ i 1) (conj ls [u v c])
                                 (assoc ad u (conj (nth ad u) [v c]) v (conj (nth ad v) [u c]))))))
        root 0
        dist (loop [it 0 ds (assoc (vec (repeat B INFLL)) root 0) dn (vec (repeat B false))]
               (if (>= it B) ds
                 (let [u (loop [i 0 best -1]
                           (if (>= i B) best
                             (recur (+ i 1) (if (and (not (nth dn i))
                                                     (or (= best -1) (< (nth ds i) (nth ds best)))) i best))))]
                   (if (or (= u -1) (= (nth ds u) INFLL)) ds
                     (recur (+ it 1)
                            (reduce (fn [dd [vv cc]] (if (< (+ (nth dd u) cc) (nth dd vv)) (assoc dd vv (+ (nth dd u) cc)) dd))
                                    ds (nth adj u))
                            (assoc dn u true))))))
        rp (mapv (fn [b] (cond (= b root) -1
                              (= (nth dist b) INFLL) -2
                              :else (loop [es (nth adj b) best -1]
                                      (if (empty? es) best
                                        (let [[nb cc] (first es)]
                                          (recur (rest es)
                                                 (if (and (= (+ (nth dist nb) cc) (nth dist b))
                                                          (or (= best -1) (< nb best))) nb best)))))))
                 (range B))
        des (mapv (fn [[u v _]] (cond (< (nth dist u) (nth dist v)) u
                                     (< (nth dist v) (nth dist u)) v
                                     :else (min u v))) links)]
    (into [(str root)] (concat (map str rp) (map str des)))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
