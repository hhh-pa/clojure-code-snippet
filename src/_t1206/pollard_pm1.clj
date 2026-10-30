;; pollard_pm1: Pollard p-1 factorization. Smooth p-1 extraction.
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
(defn gcd [a b] (if (zero? b) (Math/abs a) (recur b (mod a b))))
(defn -main [input]
  (let [toks (tokenize input #"\s+")
        n (Long/parseLong (nth toks 0)) B (Long/parseLong (nth toks 1))]
    (if (<= n 1) (println "NONE")
      (loop [a 2 k 2]
        (if (> k B)
          (let [g (gcd (dec a) n)] (if (and (> g 1) (< g n)) (println g) (println "NONE")))
          (let [a2 (modpow a k n) g (gcd (dec a2) n)]
            (if (and (> g 1) (< g n)) (println g) (recur a2 (inc k)))))))))
(-main)

