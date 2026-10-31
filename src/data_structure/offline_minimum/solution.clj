;; offline-minimum: Off-Line-Minimum via DSU next-slot (CLRS Ch 19 Prob).
;; Inserts in increasing value order each claim the earliest unclaimed
;; later EXTRACT-MIN via union-find. Line1: q; ops 1 x / 2. One line
;; per extract (value or EMPTY). Pure Clojure.

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

(defn dsu-find [p x]
  (loop [p p x x trail []]
    (if (= (nth p x) x)
      [(reduce (fn [pp t] (assoc pp t x)) p trail) x]
      (recur p (nth p x) (conj trail x)))))

(defn solve [toks]
  (let [q (read-int (nth toks 0))
        [is-ext val _] (loop [k 0 i 1 ie [] vv []]
                         (if (>= k q) [ie vv i]
                           (let [op (read-int (nth toks i))]
                             (if (= op 1)
                               (recur (+ k 1) (+ i 2) (conj ie false) (conj vv (read-int (nth toks (+ i 1)))))
                               (recur (+ k 1) (+ i 1) (conj ie true) (conj vv 0))))))
        epos (filterv is-ext (range q))
        m (count epos)
        ins (sort (for [i (range q) :when (not (nth is-ext i))] [(nth val i) i]))
        [ans has _] (loop [rs (seq ins) ans (vec (repeat m 0)) has (vec (repeat m false)) p (vec (range (+ m 1)))]
                      (if (nil? rs) [ans has p]
                        (let [[[v t] & more] rs
                              j0 (loop [j 0] (cond (>= j m) m (> (nth epos j) t) j :else (recur (+ j 1))))
                              [p2 j] (dsu-find p j0)]
                          (if (< j m)
                            (recur more (assoc ans j v) (assoc has j true)
                                   (let [[p3 nxt] (dsu-find p2 (+ j 1))] (assoc p3 j nxt)))
                            (recur more ans has p2)))))]
    (mapv (fn [j] (if (nth has j) (str (nth ans j)) "EMPTY")) (range m))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
