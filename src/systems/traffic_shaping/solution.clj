;; traffic-shaping: token bucket, leaky bucket, WFQ/virtual-clock.
;; Line1 policy (0 token, 1 leaky, 2 wfq).
;; 0/1: rate depth n; packets time size -> CONFORM/DROP each.
;; 2: F n; F weights; packets flow arrival size -> finish num/den per
;; packet (common denominator), then ORDER line. Pure Clojure.

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

(defn igcd [a b]
  (loop [x a y b] (if (zero? y) x (recur y (mod x y)))))

(defn solve [toks]
  (let [policy (read-int (nth toks 0))]
    (cond
      (= policy 0)
      (let [rate (read-int (nth toks 1)) depth (read-int (nth toks 2)) n (read-int (nth toks 3))]
        (loop [i 0 tokens depth last 0 first true out []]
          (if (>= i n) out
            (let [t (read-int (nth toks (+ 4 (* i 2)))) s (read-int (nth toks (+ 5 (* i 2))))
                  last2 (if first t last)
                  tokens2 (min depth (+ tokens (* rate (- t last2))))]
              (if (>= tokens2 s)
                (recur (+ i 1) (- tokens2 s) t false (conj out "CONFORM"))
                (recur (+ i 1) tokens2 t false (conj out "DROP")))))))
      (= policy 1)
      (let [rate (read-int (nth toks 1)) depth (read-int (nth toks 2)) n (read-int (nth toks 3))]
        (loop [i 0 content 0 last 0 first true out []]
          (if (>= i n) out
            (let [t (read-int (nth toks (+ 4 (* i 2)))) s (read-int (nth toks (+ 5 (* i 2))))
                  last2 (if first t last)
                  content2 (max 0 (- content (* rate (- t last2))))]
              (if (<= (+ content2 s) depth)
                (recur (+ i 1) (+ content2 s) t false (conj out "CONFORM"))
                (recur (+ i 1) content2 t false (conj out "DROP")))))))
      :else
      (let [F (read-int (nth toks 1)) n (read-int (nth toks 2))
            w (mapv (fn [f] (read-int (nth toks (+ 3 f)))) (range F))
            L (reduce (fn [acc x] (* (quot acc (igcd acc x)) x)) 1 w)
            pkts (mapv (fn [i] [(read-int (nth toks (+ 3 F (* i 3))))
                                (read-int (nth toks (+ 4 F (* i 3))))
                                (read-int (nth toks (+ 5 F (* i 3))))]) (range n))
            [fnum _] (loop [i 0 prev (vec (repeat F 0)) acc []]
                       (if (>= i n) [acc prev]
                         (let [[fl ar sz] (nth pkts i)
                               start (max (* ar L) (nth prev fl))
                               fin (+ start (* sz (quot L (nth w fl))))]
                           (recur (+ i 1) (assoc prev fl fin) (conj acc fin)))))
            ord (sort (fn [x y] (let [fx (nth fnum x) fy (nth fnum y)]
                                  (if (not= fx fy) (< fx fy)
                                    (if (not= (nth (nth pkts x) 0) (nth (nth pkts y) 0))
                                      (< (nth (nth pkts x) 0) (nth (nth pkts y) 0)) (< x y)))))
                      (range n))]
        (conj (mapv (fn [f] (str f "/" L)) fnum)
              (str "ORDER" (apply str (map (fn [i] (str " " i)) ord))))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
