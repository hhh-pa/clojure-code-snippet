;; bsp-tree: autopartition BSP. The first segment is the splitting
;; line; strictly one-sided segments recurse, straddling or touching
;; ones stay at the node. Queries walk by orientation until missing
;; child or on-line ('='). Preorder dump then query paths.
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
(defn orient [sp px py]
  (- (* (- (nth sp 2) (nth sp 0)) (- py (nth sp 1)))
     (* (- (nth sp 3) (nth sp 1)) (- px (nth sp 0)))))
(defn build [segs ids]
  (if (empty? ids) [-1 []]
    (let [sp (nth segs (first ids))
          sides (mapv (fn [i]
                        (let [s (nth segs i)
                              o1 (orient sp (nth s 0) (nth s 1))
                              o2 (orient sp (nth s 2) (nth s 3))]
                          (cond (and (neg? o1) (neg? o2)) :l
                                (and (pos? o1) (pos? o2)) :r
                                :else :at))) (rest ids))
          left (vec (keep-indexed (fn [k sd] (when (= sd :l) (nth (rest ids) k))) sides))
          right (vec (keep-indexed (fn [k sd] (when (= sd :r) (nth (rest ids) k))) sides))
          at (vec (cons (first ids) (keep-indexed (fn [k sd] (when (= sd :at) (nth (rest ids) k))) sides)))
          [li lnodes] (build segs left)
          [ri rnodes] (build segs right)
          off (inc (count lnodes))
          shift (fn [t o] (mapv (fn [nd] (assoc nd 1 (if (= (nth nd 1) -1) -1 (+ (nth nd 1) o))
                                                   2 (if (= (nth nd 2) -1) -1 (+ (nth nd 2) o)))) t))
          me [[sp (if (= li -1) -1 1) (if (= ri -1) -1 off) at]]]
      [0 (vec (concat me (shift lnodes 1) (shift rnodes off)))])))
(defn -main []
  (let [nums (mapv parse-token (tokenize (slurp *in*)))
        n (nth nums 0)
        segs (vec (for [i (range n)] [(nth nums (+ 1 (* 4 i))) (nth nums (+ 2 (* 4 i))) (nth nums (+ 3 (* 4 i))) (nth nums (+ 4 (* 4 i)))]))
        [_ nodes] (build segs (vec (range n)))
        qbase (+ 1 (* 4 n))
        q (nth nums qbase)
        out (java.lang.StringBuilder.)]
    (.append out (str (count nodes) "\n"))
    (doseq [[sp l r at] nodes]
      (.append out (str (nth sp 0) " " (nth sp 1) " " (nth sp 2) " " (nth sp 3) " " l " " r " " (count at)))
      (doseq [i at] (.append out (str " " i)))
      (.append out "\n"))
    (dotimes [i q]
      (let [x (nth nums (+ qbase 1 (* 2 i))) y (nth nums (+ qbase 2 (* 2 i)))
            [path node] (loop [node 0 path []]
                          (let [o (orient (nth (nth nodes node) 0) x y)]
                            (cond (neg? o) (let [l (nth (nth nodes node) 1)] (if (= l -1) [path node] (recur l (conj path "L"))))
                                  (pos? o) (let [r (nth (nth nodes node) 2)] (if (= r -1) [path node] (recur r (conj path "R"))))
                                  :else [(conj path "=") node])))]
        (.append out (str (if (empty? path) "-" (clojure.string/join " " path)) " " node "\n"))))
    (print (str out))))
(-main)
