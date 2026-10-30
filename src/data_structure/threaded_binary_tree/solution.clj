;; threaded-binary-tree: double-threaded BST (Karumanchi Ch 6).
;; Null links store inorder predecessor/successor; traversal uses only
;; threads (no stack/recursion). Line1: n q; n keys; ops: 1 x pred,
;; 2 x succ, 3 inorder line. Pure Clojure.

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

(defn tbt-insert [t x]
  (let [n (count (:key t))
        t2 (assoc t :key (conj (:key t) x) :l (conj (:l t) -1) :r (conj (:r t) -1)
                 :lt (conj (:lt t) true) :rt (conj (:rt t) true))]
    (if (= (:root t2) -1) (assoc t2 :root n)
      (loop [t3 t2 cur (:root t2)]
        (cond (= x (nth (:key t3) cur))
              (assoc t3 :key (pop (:key t3)) :l (pop (:l t3)) :r (pop (:r t3))
                     :lt (pop (:lt t3)) :rt (pop (:rt t3)))
              (< x (nth (:key t3) cur))
              (if (nth (:lt t3) cur)
                (let [l1 (assoc (:l t3) n (nth (:l t3) cur))
                      l2 (assoc l1 cur n)
                      r1 (assoc (:r t3) n cur)
                      t4 (assoc t3 :l l2 :r r1 :lt (assoc (:lt t3) cur false))
                      pl (nth (:l t4) n)]
                  (if (and (not= pl -1) (nth (:rt t4) pl))
                    (assoc t4 :r (assoc (:r t4) pl n)) t4))
                (recur t3 (nth (:l t3) cur)))
              :else
              (if (nth (:rt t3) cur)
                (let [r1 (assoc (:r t3) n (nth (:r t3) cur))
                      r2 (assoc r1 cur n)
                      l1 (assoc (:l t3) n cur)
                      t4 (assoc t3 :l l1 :r r2 :rt (assoc (:rt t3) cur false))
                      sr (nth (:r t4) n)]
                  (if (and (not= sr -1) (nth (:lt t4) sr))
                    (assoc t4 :l (assoc (:l t4) sr n)) t4))
                (recur t3 (nth (:r t3) cur))))))))

(defn pred-node [t i]
  (if (nth (:lt t) i) (nth (:l t) i)
    (loop [j (nth (:l t) i)] (if (nth (:rt t) j) j (recur (nth (:r t) j))))))

(defn succ-node [t i]
  (if (nth (:rt t) i) (nth (:r t) i)
    (loop [j (nth (:r t) i)] (if (nth (:lt t) j) j (recur (nth (:l t) j))))))

(defn tbt-find [t x]
  (loop [cur (:root t) lo -1 hi -1]
    (cond (= cur -1) [-1 lo hi]
          (= x (nth (:key t) cur)) [cur lo hi]
          (< x (nth (:key t) cur))
          (if (nth (:lt t) cur) [-1 lo cur] (recur (nth (:l t) cur) lo cur))
          :else (if (nth (:rt t) cur) [-1 cur hi] (recur (nth (:r t) cur) cur hi)))))

(defn inorder-keys [t]
  (if (= (:root t) -1) []
    (loop [cur (loop [c (:root t)] (if (nth (:lt t) c) c (recur (nth (:l t) c)))) acc []]
      (if (= cur -1) acc
        (recur (succ-node t cur) (conj acc (nth (:key t) cur)))))))

(defn solve [toks]
  (let [n (read-int (nth toks 0)) q (read-int (nth toks 1))
        t (loop [i 0 t {:key [] :l [] :r [] :lt [] :rt [] :root -1}]
            (if (>= i n) t (recur (+ i 1) (tbt-insert t (read-int (nth toks (+ 2 i)))))))]
    (loop [k 0 i (+ 2 n) out []]
      (if (>= k q) out
        (let [op (read-int (nth toks i))]
          (cond
            (= op 1) (let [x (read-int (nth toks (+ i 1))) [f lo _] (tbt-find t x)
                           p (if (= f -1) lo (pred-node t f))]
                       (recur (+ k 1) (+ i 2) (conj out (if (= p -1) "NONE" (str (nth (:key t) p))))))
            (= op 2) (let [x (read-int (nth toks (+ i 1))) [f _ hi] (tbt-find t x)
                           s (if (= f -1) hi (succ-node t f))]
                       (recur (+ k 1) (+ i 2) (conj out (if (= s -1) "NONE" (str (nth (:key t) s))))))
            (= op 3) (recur (+ k 1) (+ i 1) (conj out (apply str (interpose " " (map str (inorder-keys t))))))
            :else (recur (+ k 1) (+ i 1) out)))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
