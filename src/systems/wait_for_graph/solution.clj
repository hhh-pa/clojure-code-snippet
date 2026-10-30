;; wait-for-graph: deadlock detection via Kosaraju SCC (Tanenbaum MOS 6.4).
;; Input: n m; m wait edges u v (0-based, u waits for v).
;; Output: NO DEADLOCK, or DEADLOCK + sorted cycle nodes. Pure Clojure.

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

(defn dfs-finish [g s seen order]
  (loop [st [[s 0]] seen (assoc seen s true) order order]
    (if (empty? st) [seen order]
      (let [[u k] (peek st) nbrs (nth g u)]
        (if (< k (count nbrs))
          (let [v (nth nbrs k) st2 (assoc st (- (count st) 1) [u (+ k 1)])]
            (if (nth seen v)
              (recur st2 seen order)
              (recur (conj st2 [v 0]) (assoc seen v true) order)))
          (recur (pop st) seen (conj order u)))))))

(defn dfs-label [gr s c comp]
  (loop [st [s] comp (assoc comp s c)]
    (if (empty? st) comp
      (let [u (peek st) st2 (pop st)]
        (recur (into st2 (filter (fn [v] (= (nth comp v) -1)) (nth gr u)))
               (reduce (fn [cc v] (if (= (nth cc v) -1) (assoc cc v c) cc)) comp (nth gr u)))))))

(defn solve [toks]
  (let [n (read-int (nth toks 0)) m (read-int (nth toks 1))
        [g gr self-loop] (loop [i 0 g (vec (repeat n [])) gr (vec (repeat n [])) sl (vec (repeat n false))]
                           (if (>= i m) [g gr sl]
                             (let [u (read-int (nth toks (+ 2 (* i 2)))) v (read-int (nth toks (+ 3 (* i 2))))]
                               (if (or (< u 0) (>= u n) (< v 0) (>= v n))
                                 (recur (+ i 1) g gr sl)
                                 (recur (+ i 1) (assoc g u (conj (nth g u) v))
                                        (assoc gr v (conj (nth gr v) u))
                                        (assoc sl u (or (nth sl u) (= u v))))))))
        [seen order] (loop [s 0 seen (vec (repeat n false)) order []]
                       (if (>= s n) [seen order]
                         (if (nth seen s) (recur (+ s 1) seen order)
                           (let [[seen2 order2] (dfs-finish g s seen order)]
                             (recur (+ s 1) seen2 order2)))))
        [comp nc] (loop [os (reverse order) comp (vec (repeat n -1)) nc 0]
                    (if (empty? os) [comp nc]
                      (let [s (first os)]
                        (if (not= (nth comp s) -1)
                          (recur (rest os) comp nc)
                          (recur (rest os) (dfs-label gr s nc comp) (+ nc 1))))))
        sizes (reduce (fn [sz c] (assoc sz c (+ (nth sz c) 1))) (vec (repeat nc 0)) comp)
        cyc (filterv (fn [u] (or (> (nth sizes (nth comp u)) 1) (nth self-loop u))) (range n))]
    (if (empty? cyc) "NO DEADLOCK"
      (str "DEADLOCK" (apply str (map (fn [u] (str " " u)) cyc))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (println (solve toks))))
