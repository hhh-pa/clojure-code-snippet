;; d-ary-heap: min-heap with d children per node (CLRS Ch 6 Ex).
;; Line1: d q. Ops: 1 x insert, 2 extract-min, 3 peek. Pure Clojure.

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

(defn swim [d a k]
  (loop [a a k k]
    (if (zero? k) a
      (let [p (quot (- k 1) d)]
        (if (>= (nth a k) (nth a p)) a
          (recur (assoc a k (nth a p) p (nth a k)) p))))))

(defn sink [d a k]
  (loop [a a k k]
    (let [n (count a)
          m (loop [j 1 m k]
              (if (> j d) m
                (let [c (+ (* d k) j)]
                  (recur (+ j 1) (if (and (< c n) (< (nth a c) (nth a m))) c m)))))]
      (if (= m k) a
        (recur (assoc a k (nth a m) m (nth a k)) m)))))

(defn solve [toks]
  (let [d0 (read-int (nth toks 0)) q (read-int (nth toks 1))
        d (if (< d0 2) 2 d0)]
    (loop [k 0 i 2 a [] out []]
      (if (>= k q) out
        (let [op (read-int (nth toks i))]
          (cond
            (= op 1) (let [x (read-int (nth toks (+ i 1))) a2 (conj a x)]
                       (recur (+ k 1) (+ i 2) (swim d a2 (- (count a2) 1)) out))
            (= op 2) (if (empty? a) (recur (+ k 1) (+ i 1) a (conj out "EMPTY"))
                       (let [o (nth a 0)
                             a2 (if (= (count a) 1) [] (sink d (assoc (pop a) 0 (peek a)) 0))]
                         (recur (+ k 1) (+ i 1) a2 (conj out (str o)))))
            (= op 3) (recur (+ k 1) (+ i 1) a (conj out (if (empty? a) "EMPTY" (str (nth a 0)))))
            :else (recur (+ k 1) (+ i 1) a out)))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
