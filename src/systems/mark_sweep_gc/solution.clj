;; mark-sweep-gc: conservative mark-and-sweep collector (Tanenbaum MOS).
;; Input: n r; r roots; n lines k refs.... Output: sorted freed ids,
;; or NONE. Pure Clojure.

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

(defn solve [toks]
  (let [n (read-int (nth toks 0)) r (read-int (nth toks 1))
        roots (mapv (fn [i] (read-int (nth toks (+ 2 i)))) (range r))
        [refs _] (loop [i 0 pos (+ 2 r) acc []]
                   (if (>= i n) [acc pos]
                     (let [k (read-int (nth toks pos))]
                       (recur (+ i 1) (+ pos 1 k)
                              (conj acc (mapv (fn [j] (read-int (nth toks (+ pos 1 j)))) (range k)))))))
        marked (loop [rs (seq roots) mk (vec (repeat n false))]
                 (if (nil? rs) mk
                   (let [rt (first rs)]
                     (if (or (< rt 0) (>= rt n) (nth mk rt))
                       (recur (next rs) mk)
                       (recur (next rs)
                              (loop [st [rt] mk2 (assoc mk rt true)]
                                (if (empty? st) mk2
                                  (let [u (peek st)
                                        fresh (filterv (fn [v] (and (>= v 0) (< v n) (not (nth mk2 v)))) (nth refs u))]
                                    (recur (into (pop st) fresh)
                                           (reduce (fn [mm v] (assoc mm v true)) mk2 fresh))))))))))
        freed (filterv (fn [i] (not (nth marked i))) (range n))]
    (if (empty? freed) "NONE"
      (apply str (interpose " " (map str freed))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (println (solve toks))))
