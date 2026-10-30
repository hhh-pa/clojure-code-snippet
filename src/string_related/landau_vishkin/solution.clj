;; landau-vishkin: k-difference approximate string matching via diagonal DP.
;; L[d][e] holds the furthest pattern row on diagonal e=j-i reachable with
;; d errors; each cell extends by a longest-common-extension jump. Matches
;; end where some row reaches the full pattern. O(kn) cells, naive LCE scan.
;; Input: pattern text k. Output: match count, then 0-based end positions.
(def digit-values (zipmap "0123456789" (range)))
(def separators #{\space \tab \newline \return \formfeed \u000B})
(defn tokenize [s]
  (let [done (reduce (fn [{:keys [tokens current]} ch]
                       (if (contains? separators ch)
                         (if (empty? current) {:tokens tokens :current []}
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
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (>= acc (quot (+ min-long d) 10))) (- (* acc 10) d))))) 0 rest-chs)
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs)))))))
(def NEG -1000000000)
(defn lce [p t m n i j]
  (loop [i i j j c 0]
    (if (and (< i m) (< j n) (= (nth p i) (nth t j))) (recur (inc i) (inc j) (inc c)) c)))
(defn -main []
  (let [toks (tokenize (slurp *in*))
        p (vec (nth toks 0)) t (vec (nth toks 1)) k (parse-token (nth toks 2))
        m (count p) n (count t) off (+ m 2) sz (+ m n 5)
        matched (boolean-array n false)]
    (loop [d 0 prev (vec (repeat sz NEG))]
      (when (<= d k)
        (let [cur (loop [e (- m) cur (vec (repeat sz NEG))]
                    (if (> e n) cur
                      (let [r (if (zero? d)
                                (when (and (>= e 0) (<= e n)) 0)
                                (let [a (+ (nth prev (+ e off)) 1) b (nth prev (+ e -1 off)) c (+ (nth prev (+ e 1 off)) 1)
                                      r (max a (max b c))]
                                  (when (> r (quot NEG 2)) r)))]
                        (if (nil? r) (recur (inc e) cur)
                          (let [l (+ r (lce p t m n r (+ r e)))]
                            (when (>= l m) (let [end (+ m e -1)] (when (and (>= end 0) (< end n)) (aset matched end true))))
                            (recur (inc e) (assoc cur (+ e off) l)))))))]
          (recur (inc d) cur))))
    (let [ans (filterv #(aget matched %) (range n))]
      (println (count ans))
      (when (seq ans) (println (apply str (interpose " " ans)))))))
(-main)
