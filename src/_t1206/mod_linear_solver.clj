;; mod_linear_solver: modular linear equation ax≡b mod n (CLRS 31.4). All solutions.
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

(defn egcd [a b]
  (if (zero? b) [a 1 0]
    (let [[g x1 y1] (egcd b (mod a b))] [g y1 (- x1 (* (quot a b) y1))])))
(defn -main [input]
  (let [toks (tokenize input #"\s+")
        a (Long/parseLong (nth toks 0)) b (Long/parseLong (nth toks 1)) n (Long/parseLong (nth toks 2))]
    (cond (= n 1) (println "0")
          (<= n 0) (println "NONE")
          :else (let [a (mod a n) b (mod b n)
                      [g x _] (egcd a n)]
                  (if (not= 0 (mod b g)) (println "NONE")
                    (let [n2 (quot n g) x0 (mod (* x (quot b g)) n)
                          sols (sort (map #(mod (+ x0 (* % n2)) n) (range g)))]
                      (println g) (println (str/join " " sols))))))))
(-main)

