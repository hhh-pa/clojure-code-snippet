(require '[clojure.string :as str])
;; shortest_common_superstring: Shortest common superstring via overlap graph greedy.
;; Pure Clojure with canonical input parsing.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

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

(defn overlap [a b]
  (let [mn (min (count a) (count b))]
    (or (first (filter #(= (subs a (- (count a) %)) (subs b 0 %)) (range mn 0 -1))) 0)))
(defn -main [input]
  (let [toks (filter seq (str/split input #"\s+"))
        n (when (seq toks) (Integer/parseInt (first toks)))
        strs (vec (take n (rest toks)))]
    (when (seq strs)
      (loop [strs strs]
        (if (<= (count strs) 1) (println (first strs))
          (let [pairs (for [i (range (count strs)) j (range (count strs)) :when (not= i j)] [i j])
                scored (map (fn [[i j]] [i j (overlap (nth strs i) (nth strs j))]) pairs)
                [bi bj bov] (reduce (fn [best cur] (if (> (nth cur 2) (nth best 2)) cur best)) scored)
                merged (str (nth strs bi) (subs (nth strs bj) bov))
                next (conj (vec (keep-indexed (fn [k s] (when (and (not= k bi) (not= k bj)) s)) strs)) merged)]
            (recur next)))))))
(-main (slurp *in*))