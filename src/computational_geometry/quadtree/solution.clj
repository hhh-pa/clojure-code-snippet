;; quadtree: point-region quadtree over [0,S)x[0,S). A node
;; holding more than cap points splits into SW,SE,NW,NE quadrants
;; (no split at size 1). Preorder dump, then pruned rectangle
;; counting queries answered by tree traversal.
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
(declare build)
(defn build [px py cap ids x y s]
  (if (or (<= (count ids) cap) (= s 1))
    [0 [{:leaf true :x x :y y :s s :c [-1 -1 -1 -1] :ids (vec ids)}]]
    (let [h (quot s 2)
          qs (reduce (fn [q i]
                       (let [k (+ (if (< (nth px i) (+ x h)) 0 1)
                                  (if (< (nth py i) (+ y h)) 0 2))]
                         (assoc q k (conj (nth q k) i))))
                     [[] [] [] []] ids)
          bs [[x y h] [(+ x h) y (- s h)] [x (+ y h) h] [(+ x h) (+ y h) (- s h)]]
          parts (mapv (fn [k] (if (empty? (nth qs k)) [-1 []] (let [[bx by bs2] (nth bs k)] (build px py cap (nth qs k) bx by bs2)))) (range 4))
          offs (loop [k 0 off 1 acc []]
                 (if (>= k 4) acc
                   (recur (inc k) (+ off (count (second (nth parts k)))) (conj acc (if (= (first (nth parts k)) -1) -1 off)))))
          shift (fn [nodes o] (mapv (fn [nd] (if (:leaf nd) nd (assoc nd :c (mapv #(if (= % -1) -1 (+ % o)) (:c nd))))) nodes))
          nodes (vec (concat [{:leaf false :x x :y y :s s :c offs :ids []}]
                             (mapcat (fn [k] (shift (second (nth parts k)) (nth offs k)))
                                     (filter #(not= (nth offs %) -1) (range 4)))))]
      [0 nodes])))
(defn qcount [nodes px py ni x1 y1 x2 y2]
  (let [nd (nth nodes ni)]
    (if (:leaf nd)
      (count (filter (fn [i] (and (<= x1 (nth px i) x2) (<= y1 (nth py i) y2))) (:ids nd)))
      (let [h (quot (:s nd) 2)
            regs [[(:x nd) (:y nd) h] [(+ (:x nd) h) (:y nd) (- (:s nd) h)]
                  [(:x nd) (+ (:y nd) h) h] [(+ (:x nd) h) (+ (:y nd) h) (- (:s nd) h)]]]
        (reduce + 0 (keep-indexed
                     (fn [k c]
                       (when (not= c -1)
                         (let [[rx ry rs] (nth regs k)]
                           (when-not (or (> rx x2) (> ry y2) (< (+ rx rs -1) x1) (< (+ ry rs -1) y1))
                             (qcount nodes px py c x1 y1 x2 y2)))))
                     (:c nd)))))))
(defn -main []
  (let [nums (mapv parse-token (tokenize (slurp *in*)))
        n (nth nums 0) S (nth nums 1) cap (nth nums 2)
        px (vec (for [i (range n)] (nth nums (+ 3 (* 2 i)))))
        py (vec (for [i (range n)] (nth nums (+ 4 (* 2 i)))))
        [_ nodes] (build px py cap (vec (range n)) 0 0 S)
        qbase (+ 3 (* 2 n))
        q (nth nums qbase)
        out (java.lang.StringBuilder.)]
    (.append out (str (count nodes) "\n"))
    (doseq [nd nodes]
      (if (:leaf nd)
        (do (.append out (str "L " (:x nd) " " (:y nd) " " (:s nd) " " (count (:ids nd))))
            (doseq [i (:ids nd)] (.append out (str " " i)))
            (.append out "\n"))
        (.append out (str "I " (:x nd) " " (:y nd) " " (:s nd) " " (nth (:c nd) 0) " " (nth (:c nd) 1) " " (nth (:c nd) 2) " " (nth (:c nd) 3) "\n"))))
    (dotimes [i q]
      (let [x1 (nth nums (+ qbase 1 (* 4 i))) y1 (nth nums (+ qbase 2 (* 4 i)))
            x2 (nth nums (+ qbase 3 (* 4 i))) y2 (nth nums (+ qbase 4 (* 4 i)))]
        (.append out (str (qcount nodes px py 0 x1 y1 x2 y2) "\n"))))
    (print (str out))))
(-main)
