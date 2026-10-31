;; cpu-scheduling: FCFS, SJF, Round-Robin (Tanenbaum MOS 2.4).
;; Line1: policy (0 FCFS, 1 SJF, 2 RR). RR: line2 quantum. Then n,
;; n arrival/burst lines. Output: completion order, total wait. Pure Clojure.

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

(defn sim-fcfs [arr burst idx n]
  (loop [ks (seq idx) now 0 comp (vec (repeat n -1)) order []]
    (if (nil? ks) [comp order]
      (let [id (first ks) now2 (max now (nth arr id)) end (+ now2 (nth burst id))]
        (recur (next ks) end (assoc comp id end) (conj order id))))))

(defn pick-sjf [arr burst done now n]
  (loop [id 0 pick -1]
    (if (>= id n) pick
      (if (or (nth done id) (> (nth arr id) now)) (recur (+ id 1) pick)
        (recur (+ id 1)
               (if (= pick -1) id
                 (let [better (or (< (nth burst id) (nth burst pick))
                                  (and (= (nth burst id) (nth burst pick))
                                       (or (< (nth arr id) (nth arr pick))
                                           (and (= (nth arr id) (nth arr pick)) (< id pick)))))]
                   (if better id pick))))))))

(defn sim-sjf [arr burst n]
  (loop [now 0 done (vec (repeat n false)) comp (vec (repeat n -1)) order [] left n]
    (if (zero? left) [comp order]
      (let [pick (pick-sjf arr burst done now n)]
        (if (= pick -1)
          (let [nx (loop [id 0 best -1]
                      (if (>= id n) best
                        (if (nth done id) (recur (+ id 1) best)
                          (recur (+ id 1) (if (or (= best -1) (< (nth arr id) best)) (nth arr id) best)))))]
            (recur nx done comp order left))
          (let [end (+ now (nth burst pick))]
            (recur end (assoc done pick true) (assoc comp pick end) (conj order pick) (- left 1))))))))

(defn sim-rr [arr burst idx n quantum]
  (loop [now 0 ap 0 ready [] rem burst comp (vec (repeat n -1)) order [] left n]
    (if (zero? left) [comp order]
      (let [[ap2 ready2] (loop [ap ap rd ready]
                           (if (and (< ap (count idx)) (<= (nth arr (nth idx ap)) now))
                             (recur (+ ap 1) (conj rd (nth idx ap))) [ap rd]))]
        (if (empty? ready2)
          (recur (nth arr (nth idx ap2)) ap2 ready2 rem comp order left)
          (let [id (first ready2) rest-r (vec (rest ready2))
                run (min (nth rem id) quantum) now2 (+ now run) rem2 (assoc rem id (- (nth rem id) run))
                [ap3 ready3] (loop [ap ap2 rd rest-r]
                               (if (and (< ap (count idx)) (<= (nth arr (nth idx ap)) now2))
                                 (recur (+ ap 1) (conj rd (nth idx ap))) [ap rd]))]
            (if (pos? (nth rem2 id))
              (recur now2 ap3 (conj ready3 id) rem2 comp order left)
              (recur now2 ap3 ready3 rem2 (assoc comp id now2) (conj order id) (- left 1)))))))))

(defn solve [toks]
  (let [policy (read-int (nth toks 0))
        [quantum base] (if (= policy 2) [(read-int (nth toks 1)) 2] [1 1])
        n (read-int (nth toks base))
        arr (mapv (fn [i] (read-int (nth toks (+ base 1 (* i 2))))) (range n))
        burst (mapv (fn [i] (read-int (nth toks (+ base 2 (* i 2))))) (range n))
        idx (vec (sort (fn [x y] (if (not= (nth arr x) (nth arr y)) (< (nth arr x) (nth arr y)) (< x y))) (range n)))
        [comp order] (cond (= policy 0) (sim-fcfs arr burst idx n)
                           (= policy 1) (sim-sjf arr burst n)
                           :else (sim-rr arr burst idx n quantum))
        tot (reduce + (map (fn [i] (- (nth comp i) (nth arr i) (nth burst i))) (range n)))]
    [(apply str (interpose " " (map str order))) (str tot)]))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
