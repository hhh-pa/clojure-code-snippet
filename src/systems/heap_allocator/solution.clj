;; heap-allocator: implicit free list, first-fit, boundary-tag coalescing.
;; Line1: H q. Block = 8 overhead + payload; min block 16.
;; Ops: 1 s malloc (payload offset/FAIL), 2 p free (OK/FAIL), 3 layout.
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

(defn vec-insert [v k x]
  (vec (concat (subvec v 0 k) [x] (subvec v k))))

(defn vec-remove [v k]
  (vec (concat (subvec v 0 k) (subvec v (+ k 1)))))

(defn solve [toks]
  (let [H (read-int (nth toks 0)) q (read-int (nth toks 1))]
    (loop [k 0 i 2 sizes (if (>= H 16) [H] []) isfree (if (>= H 16) [true] []) alloc {} out []]
      (if (>= k q) out
        (let [op (read-int (nth toks i))]
          (cond
            (= op 1) (let [s (read-int (nth toks (+ i 1)))]
                       (if (< s 1) (recur (+ k 1) (+ i 2) sizes isfree alloc (conj out "FAIL"))
                         (let [need (+ s 8)
                               pick (loop [kk 0] (cond (>= kk (count sizes)) -1
                                                         (and (nth isfree kk) (>= (nth sizes kk) need)) kk
                                                         :else (recur (+ kk 1))))]
                           (if (= pick -1) (recur (+ k 1) (+ i 2) sizes isfree alloc (conj out "FAIL"))
                             (let [leftover (- (nth sizes pick) need)
                                   b (reduce + 0 (subvec sizes 0 pick))
                                   [sizes2 isfree2] (if (>= leftover 16)
                                                     [(vec-insert (assoc sizes pick need) (+ pick 1) leftover)
                                                      (vec-insert isfree (+ pick 1) true)]
                                                     [sizes isfree])]
                               (recur (+ k 1) (+ i 2) sizes2 (assoc isfree2 pick false)
                                      (assoc alloc (+ b 4) (nth sizes2 pick)) (conj out (str (+ b 4)))))))))
            (= op 2) (let [p (read-int (nth toks (+ i 1)))]
                       (if (not (contains? alloc p))
                         (recur (+ k 1) (+ i 2) sizes isfree alloc (conj out "FAIL"))
                         (let [b (- p 4)
                               kk (loop [kk 0 acc 0]
                                    (if (= acc b) kk (recur (+ kk 1) (+ acc (nth sizes kk)))))
                               isfree2 (assoc isfree kk true)
                               [sizes3 isfree3] (if (and (< (+ kk 1) (count sizes)) (nth isfree2 (+ kk 1)))
                                                 [(vec-remove (assoc sizes kk (+ (nth sizes kk) (nth sizes (+ kk 1)))) (+ kk 1))
                                                  (vec-remove isfree2 (+ kk 1))]
                                                 [sizes isfree2])
                               [sizes4 isfree4] (if (and (pos? kk) (nth isfree3 (- kk 1)))
                                                 [(vec-remove (assoc sizes3 (- kk 1) (+ (nth sizes3 (- kk 1)) (nth sizes3 kk))) kk)
                                                  (vec-remove isfree3 kk)]
                                                 [sizes3 isfree3])]
                           (recur (+ k 1) (+ i 2) sizes4 isfree4 (dissoc alloc p) (conj out "OK")))))
            (= op 3) (recur (+ k 1) (+ i 1) sizes isfree alloc
                            (conj out (apply str (interpose " " (mapv (fn [kk] (str (if (nth isfree kk) "F" "A") (nth sizes kk))) (range (count sizes)))))))
            :else (recur (+ k 1) (+ i 1) sizes isfree alloc out)))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
