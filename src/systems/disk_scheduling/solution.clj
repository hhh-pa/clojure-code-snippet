;; disk-scheduling: FCFS SSTF SCAN C-SCAN LOOK C-LOOK (Tanenbaum MOS 2.4).
;; Input: head maxc n; n requests. Output 6 lines:
;; "<POLICY> <total> <service order>". Pure Clojure.

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

(defn absv [x] (if (neg? x) (- x) x))

(defn line-out [name tot ord]
  (str name " " tot (apply str (map (fn [v] (str " " v)) ord))))

(defn sim-sstf [req head n]
  (loop [k 0 h head done (vec (repeat n false)) tot 0 ord []]
    (if (>= k n) [tot ord]
      (let [pick (loop [i 0 best -1]
                   (if (>= i n) best
                     (if (nth done i) (recur (+ i 1) best)
                       (recur (+ i 1)
                              (if (= best -1) i
                                (let [di (absv (- (nth req i) h)) db (absv (- (nth req best) h))]
                                  (if (or (< di db)
                                          (and (= di db)
                                               (or (< (nth req i) (nth req best))
                                                   (and (= (nth req i) (nth req best)) (< i best)))))
                                    i best)))))))]
        (recur (+ k 1) (nth req pick) (assoc done pick true)
               (+ tot (absv (- (nth req pick) h))) (conj ord (nth req pick)))))))

(defn solve [toks]
  (let [head (read-int (nth toks 0)) maxc (read-int (nth toks 1)) n (read-int (nth toks 2))
        req (mapv (fn [i] (read-int (nth toks (+ 3 i)))) (range n))
        [fcfs-tot _] (loop [i 0 h head tot 0]
                       (if (>= i n) [tot nil]
                         (recur (+ i 1) (nth req i) (+ tot (absv (- (nth req i) h))))))
        [sstf-tot sstf-ord] (sim-sstf req head n)
        up (vec (sort (filterv (fn [r] (>= r head)) req)))
        down (vec (sort (filterv (fn [r] (< r head)) req)))
        up? (seq up) down? (seq down)
        scan-tot (+ (- maxc head) (if down? (- maxc (nth down 0)) 0))
        cscan-tot (+ (- maxc head) maxc (if down? (nth down (- (count down) 1)) 0))
        look-tot (cond (and up? down?) (+ (- (nth up (- (count up) 1)) head)
                                          (- (nth up (- (count up) 1)) (nth down 0)))
                       up? (- (nth up (- (count up) 1)) head)
                       down? (- head (nth down 0)) :else 0)
        clook-tot (cond (and up? down?) (+ (- (nth up (- (count up) 1)) head)
                                           (- (nth up (- (count up) 1)) (nth down 0))
                                           (- (nth down (- (count down) 1)) (nth down 0)))
                        up? (- (nth up (- (count up) 1)) head)
                        down? (+ (- head (nth down 0)) (- (nth down (- (count down) 1)) (nth down 0)))
                        :else 0)
        down-rev (vec (reverse down))]
    [(line-out "FCFS" fcfs-tot req)
     (line-out "SSTF" sstf-tot sstf-ord)
     (line-out "SCAN" scan-tot (into up down-rev))
     (line-out "CSCAN" cscan-tot (into up down))
     (line-out "LOOK" look-tot (into up down-rev))
     (line-out "CLOOK" clook-tot (into up down))]))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
