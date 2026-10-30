;; tonelli_shanks: Tonelli-Shanks modular sqrt. O(log^2 p).
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
  (loop [r (mod 1 m) b (mod b m) e e] (if (zero? e) r (recur (if (odd? e) (mod (* r b) m) r) (mod (* b b) m) (quot e 2)))))
(defn -main [input]
  (let [toks (tokenize input #"\s+")
        a (Long/parseLong (nth toks 0)) p (Long/parseLong (nth toks 1))
        a (mod a p)]
    (cond (zero? a) (println "0")
          (= p 2) (println a)
          (not= 1 (modpow a (quot (dec p) 2) p)) (println "NONE")
          :else (let [[q s] (loop [q (dec p) s 0] (if (even? q) (recur (quot q 2) (inc s)) [q s]))]
                  (if (= s 1) (let [r (modpow a (quot (+ p 1) 4) p)] (println (min r (- p r))))
                    (let [z (loop [z 2] (if (= (dec p) (modpow z (quot (dec p) 2) p)) z (recur (inc z))))
                          c (modpow z q p) r (modpow a (quot (inc q) 2) p) t (modpow a q p)]
                      (loop [c c r r t t m s]
                        (if (= t 1) (println (min r (- p r)))
                          (let [i (loop [i 1 t2 (mod (* t t) p)] (if (= t2 1) i (recur (inc i) (mod (* t2 t2) p))))
                                b (modpow c (bit-shift-left 1 (- m i 1)) p)]
                            (recur (mod (* b b) p) (mod (* r b) p) (mod (* t b b) p) i))))))))))
(-main)

