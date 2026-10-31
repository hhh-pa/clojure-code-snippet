;; indexed-priority-queue: IndexMinPQ (Sedgewick Ch 2.4).
;; Binary heap with inverse index; ties broken by smaller handle.
;; Line1: n q. Ops: 1 i key insert/change, 2 min_index, 3 del_min,
;; 4 i delete, 5 i key_of. Pure Clojure.

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

(defn less? [h i j]
  (let [ki (nth (:keys h) i) kj (nth (:keys h) j)]
    (if (not= ki kj) (< ki kj) (< i j))))

(defn exch [h a b]
  (let [pa (nth (:pq h) a) pb (nth (:pq h) b)]
    (assoc h :pq (assoc (:pq h) a pb b pa)
           :qp (assoc (:qp h) pa b pb a))))

(defn swim [h k]
  (loop [h h k k]
    (if (and (> k 1) (less? h (nth (:pq h) k) (nth (:pq h) (quot k 2))))
      (recur (exch h k (quot k 2)) (quot k 2)) h)))

(defn sink [h k]
  (loop [h h k k]
    (if (<= (* 2 k) (:sz h))
      (let [j0 (* 2 k)
            j (if (and (< j0 (:sz h)) (less? h (nth (:pq h) (+ j0 1)) (nth (:pq h) j0))) (+ j0 1) j0)]
        (if (less? h (nth (:pq h) j) (nth (:pq h) k))
          (recur (exch h k j) j) h)) h)))

(defn insert-k [h i key]
  (if (not= (nth (:qp h) i) -1)
    (let [h2 (assoc h :keys (assoc (:keys h) i key)) k (nth (:qp h2) i)]
      (sink (swim h2 k) k))
    (let [sz2 (+ (:sz h) 1)
          h2 (assoc h :sz sz2 :qp (assoc (:qp h) i sz2) :pq (assoc (:pq h) sz2 i)
                   :keys (assoc (:keys h) i key))]
      (swim h2 sz2))))

(defn del-min [h]
  (let [m (nth (:pq h) 1)
        h2 (assoc (exch h 1 (:sz h)) :sz (- (:sz h) 1))
        h3 (sink h2 1)]
    [(assoc h3 :qp (assoc (:qp h3) m -1)) m]))

(defn erase-k [h i]
  (let [k (nth (:qp h) i)
        h2 (assoc (exch h k (:sz h)) :sz (- (:sz h) 1))
        k2 (nth (:qp h2) i)
        h3 (if (= k2 -1) h2 (sink (swim h2 k) k))]
    (assoc h3 :qp (assoc (:qp h3) i -1))))

(defn has? [h i] (and (>= i 0) (< i (:n h)) (not= (nth (:qp h) i) -1)))

(defn solve [toks]
  (let [n (read-int (nth toks 0)) q (read-int (nth toks 1))]
    (loop [k 0 i 2 h {:n n :sz 0 :pq (vec (repeat (+ n 1) 0)) :qp (vec (repeat n -1)) :keys (vec (repeat n 0))} out []]
      (if (>= k q) out
        (let [op (read-int (nth toks i))]
          (cond
            (= op 1) (let [idx (read-int (nth toks (+ i 1))) kk (read-int (nth toks (+ i 2)))]
                       (recur (+ k 1) (+ i 3) (if (and (>= idx 0) (< idx n)) (insert-k h idx kk) h) out))
            (= op 2) (recur (+ k 1) (+ i 1) h (conj out (if (zero? (:sz h)) "EMPTY" (str (nth (:pq h) 1)))))
            (= op 3) (if (zero? (:sz h)) (recur (+ k 1) (+ i 1) h (conj out "EMPTY"))
                       (let [[h2 m] (del-min h)] (recur (+ k 1) (+ i 1) h2 (conj out (str m)))))
            (= op 4) (let [idx (read-int (nth toks (+ i 1)))]
                       (if (has? h idx) (recur (+ k 1) (+ i 2) (erase-k h idx) (conj out "OK"))
                         (recur (+ k 1) (+ i 2) h (conj out "NONE"))))
            (= op 5) (let [idx (read-int (nth toks (+ i 1)))]
                       (recur (+ k 1) (+ i 2) h (conj out (if (has? h idx) (str (nth (:keys h) idx)) "NONE"))))
            :else (recur (+ k 1) (+ i 1) h out)))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
