;; young-tableau: m x n sorted matrix (CLRS Ch 6 Problems).
;; Line1: m n q. Ops: 1 x insert (FULL if full), 2 extract-min,
;; 3 x search (YES/NO). Pure Clojure.

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

(def INF 4000000000000000000)

(defn cell [a i j] (nth (nth a i) j))

(defn set-cell [a i j v] (assoc a i (assoc (nth a i) j v)))

(defn young-insert [t x]
  (let [m (:m t) n (:n t)]
    (if (not= (cell (:a t) (- m 1) (- n 1)) INF) [t false]
      (loop [a (set-cell (:a t) (- m 1) (- n 1) x) i (- m 1) j (- n 1)]
        (if (and (zero? i) (zero? j)) [(assoc t :a a) true]
          (let [[bi bj] (if (and (> i 0) (> (cell a (- i 1) j) (cell a i j))) [(- i 1) j] [i j])
                [bi2 bj2] (if (and (> j 0) (> (cell a i (- j 1)) (cell a bi bj))) [i (- j 1)] [bi bj])]
            (if (and (= bi2 i) (= bj2 j)) [(assoc t :a a) true]
              (recur (set-cell (set-cell a i j (cell a bi2 bj2)) bi2 bj2 (cell a i j)) bi2 bj2))))))))

(defn young-extract [t]
  (if (= (cell (:a t) 0 0) INF) [t nil]
    (let [o (cell (:a t) 0 0) m (:m t) n (:n t)]
      (loop [a (set-cell (:a t) 0 0 INF) i 0 j 0]
        (let [[si sj] (if (and (< (+ i 1) m) (< (cell a (+ i 1) j) (cell a i j))) [(+ i 1) j] [i j])
              [si2 sj2] (if (and (< (+ j 1) n) (< (cell a i (+ j 1)) (cell a si sj))) [i (+ j 1)] [si sj])]
          (if (and (= si2 i) (= sj2 j)) [(assoc t :a a) o]
            (recur (set-cell (set-cell a i j (cell a si2 sj2)) si2 sj2 INF) si2 sj2)))))))

(defn young-search [t x]
  (loop [i 0 j (- (:n t) 1)]
    (cond (or (>= i (:m t)) (< j 0)) false
          (= (cell (:a t) i j) x) true
          (> (cell (:a t) i j) x) (recur i (- j 1))
          :else (recur (+ i 1) j))))

(defn solve [toks]
  (let [m (read-int (nth toks 0)) n (read-int (nth toks 1)) q (read-int (nth toks 2))]
    (loop [k 0 i 3 t {:m m :n n :a (vec (repeat m (vec (repeat n INF))))} out []]
      (if (>= k q) out
        (let [op (read-int (nth toks i))]
          (cond
            (= op 1) (let [x (read-int (nth toks (+ i 1))) [t2 ok?] (young-insert t x)]
                       (recur (+ k 1) (+ i 2) t2 (if ok? out (conj out "FULL"))))
            (= op 2) (let [[t2 o] (young-extract t)]
                       (recur (+ k 1) (+ i 1) t2 (conj out (if (nil? o) "EMPTY" (str o)))))
            (= op 3) (let [x (read-int (nth toks (+ i 1)))]
                       (recur (+ k 1) (+ i 2) t (conj out (if (young-search t x) "YES" "NO"))))
            :else (recur (+ k 1) (+ i 1) t out)))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
