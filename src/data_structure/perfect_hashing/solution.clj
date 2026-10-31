;; perfect-hashing: FKS two-level static table (CLRS 11.5).
;; Input: n; n distinct keys; q; q queries. Deterministic grid search
;; for level-1 and collision-free level-2 parameters. Output: total
;; slots S, then YES/NO per query. Pure Clojure.

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

(def P 1000003)

(defn bucket-of [a b m k] (mod (mod (+ (* a k) b) P) m))

(defn level1 [keys m]
  (loop [a 1 best [1 0 -1]]
    (if (> a 500) best
      (let [best2 (loop [b 0 bst best]
                    (if (> b 500) bst
                      (let [cnt (reduce (fn [c k] (assoc c (bucket-of a b m k) (+ (nth c (bucket-of a b m k)) 1)))
                                        (vec (repeat m 0)) keys)
                            s (reduce + (mapv (fn [c] (* c c)) cnt))]
                        (recur (+ b 1) (if (or (= (nth bst 2) -1) (< s (nth bst 2))) [a b s] bst)))))]
        (recur (+ a 1) best2)))))

(defn collisions [bucket a b mj]
  (loop [ks (seq bucket) seen #{} col 0]
    (if (nil? ks) col
      (let [s (mod (mod (+ (* a (first ks)) b) P) mj)]
        (recur (next ks) (conj seen s) (if (contains? seen s) (+ col 1) col))))))

(defn level2 [bucket]
  (let [nj (count bucket) mj (if (> nj 1) (* nj nj) 1)]
    (loop [a 1 best [1 0 (+ nj 1)]]
      (if (> a 500) [mj (nth best 0) (nth best 1)]
        (let [best2 (loop [b 0 bst best]
                      (if (> b 500) bst
                        (let [c (collisions bucket a b mj)]
                          (recur (+ b 1) (if (< c (nth bst 2)) [a b c] bst)))))]
          (recur (+ a 1) best2))))))

(defn solve [toks]
  (let [n (read-int (nth toks 0))
        keys (mapv (fn [i] (read-int (nth toks (+ 1 i)))) (range n))
        q (read-int (nth toks (+ 1 n)))
        queries (mapv (fn [i] (read-int (nth toks (+ 2 n i)))) (range q))
        m (if (pos? n) n 1)
        [a1 b1 _] (level1 keys m)
        buckets (reduce (fn [bk k] (assoc bk (bucket-of a1 b1 m k) (conj (nth bk (bucket-of a1 b1 m k)) k)))
                        (vec (repeat m [])) keys)
        l2 (mapv level2 buckets)
        S (reduce + (mapv first l2))
        slots (mapv (fn [j] (let [[mj ba bb] (nth l2 j)]
                              (reduce (fn [sl k] (assoc sl (mod (mod (+ (* ba k) bb) P) mj)
                                                         (conj (nth sl (mod (mod (+ (* ba k) bb) P) mj)) k)))
                                      (vec (repeat mj [])) (nth buckets j))))
                    (range m))]
    (into [(str S)]
          (mapv (fn [x] (let [j (bucket-of a1 b1 m x)
                              [mj ba bb] (nth l2 j)
                              s (mod (mod (+ (* ba x) bb) P) mj)]
                          (if (some (fn [k] (= k x)) (nth (nth slots j) s)) "YES" "NO")))
                queries))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
