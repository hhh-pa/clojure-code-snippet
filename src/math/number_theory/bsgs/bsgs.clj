;; bsgs: baby-step giant-step discrete log. O(sqrt(p)).
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

(defn modpow [b e m]
  (loop [r (mod 1 m) b (mod b m) e e] (if (zero? e) r (recur (if (odd? e) (mod (* r b) m) r) (mod (* b b) m) (quot e 2)))))
(defn egcd [a b]
  (if (zero? b) [a 1 0]
    (let [[g x1 y1] (egcd b (mod a b))] [g y1 (- x1 (* (quot a b) y1))])))
(defn -main [input]
  (let [toks (tokenize input)
        g (mod (Long/parseLong (nth toks 0)) (Long/parseLong (nth toks 2)))
        h (mod (Long/parseLong (nth toks 1)) (Long/parseLong (nth toks 2)))
        p (Long/parseLong (nth toks 2))]
    (if (= h 1) (println "0")
      (let [m (long (Math/ceil (Math/sqrt p)))
            table (loop [j 0 cur 1 m2 {}] (if (>= j m) m2 (recur (inc j) (mod (* cur g) p) (if (contains? m2 cur) m2 (assoc m2 cur j)))))
            gm (modpow g m p)
            [_ ix _] (egcd gm p)
            factor (if (= 1 (first (egcd gm p))) (mod ix p) (modpow g (- p 1 m) p))]
        (loop [i 0 gamma h]
          (cond (>= i m) (println "NONE")
                (contains? table gamma) (let [ans (+ (* i m) (get table gamma))] (if (= h (modpow g ans p)) (println ans) (recur (inc i) (mod (* gamma factor) p))))
                :else (recur (inc i) (mod (* gamma factor) p))))))))
(-main (slurp *in*))

