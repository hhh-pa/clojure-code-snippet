;; modexp: modular exponentiation b^e mod m. O(log e).
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
            (reduce (fn [acc ch] (when (some? acc) (let [d (get digit-values ch)] (when (and (some? d) (<= acc (quot (- max-long d) 10))) (+ (* acc 10) d))))) 0 rest-chs))))))))

(defn modpow [b e m]
  (if (= m 1) 0
    (loop [r (mod 1 m) b (mod b m) e e]
      (if (zero? e) r (recur (if (odd? e) (mod (* r b) m) r) (mod (* b b) m) (quot e 2))))))
(defn -main [input]
  (let [toks (tokenize input #"\s+")
        vals (mapv #(Long/parseLong %) toks)]
    (cond (= 3 (count vals)) (println (modpow (nth vals 0) (nth vals 1) (nth vals 2)))
          :else (let [t (first vals)]
                  (if (and (pos? t) (= (count vals) (inc (* 3 t))))
                    (doseq [i (range t)] (println (modpow (nth vals (+ 1 (* 3 i))) (nth vals (+ 2 (* 3 i))) (nth vals (+ 3 (* 3 i))))))
                    (println (modpow (nth vals 0) (nth vals 1) (nth vals 2))))))))
(-main)

