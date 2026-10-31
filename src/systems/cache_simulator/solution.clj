;; cache-simulator: set-associative cache, LRU eviction (S,E,B).
;; Input: S E B n; n byte addresses. Output: "hits misses evictions".
;; Pure Clojure.

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

(defn solve [toks]
  (let [S (read-int (nth toks 0)) E (read-int (nth toks 1))
        B (read-int (nth toks 2)) n (read-int (nth toks 3))
        addrs (mapv (fn [i] (read-int (nth toks (+ 4 i)))) (range n))]
    (loop [i 0 sets (vec (repeat S [])) hits 0 misses 0 evict 0]
      (if (>= i n) (str hits " " misses " " evict)
        (let [addr (nth addrs i)
              block (quot addr B)
              set (mod block S)
              tag (quot block S)
              lines (nth sets set)
              at (loop [k 0] (cond (>= k (count lines)) -1
                                   (= (nth lines k) tag) k
                                   :else (recur (+ k 1))))]
          (if (>= at 0)
            (recur (+ i 1)
                   (assoc sets set (vec (cons tag (concat (subvec lines 0 at) (subvec lines (+ at 1))))))
                   (+ hits 1) misses evict)
            (let [trimmed (if (>= (count lines) E) (pop lines) lines)]
              (recur (+ i 1) (assoc sets set (vec (cons tag trimmed)))
                     hits (+ misses 1) (if (>= (count lines) E) (+ evict 1) evict)))))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (println (solve toks))))
