;; universal-hashing: multiply-shift h(x)=((a*x) mod 2^w)>>(w-k) (CLRS 11.3).
;; Line1: a w k. Then n and n keys. Output h per key. Pure Clojure.

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

(defn read-uint [tok]
  (let [v (parse-token tok)] v))

(defn solve [toks]
  (let [a (read-uint (nth toks 0)) w (read-int (nth toks 1)) k (read-int (nth toks 2))
        n (read-int (nth toks 3))
        mask (cond (>= w 64) -1 (= w 0) 0 :else (dec (bit-shift-left 1 w)))]
    (loop [i 0 out []]
      (if (>= i n) out
        (let [x (read-uint (nth toks (+ 4 i)))
              p (bit-and (unchecked-multiply a x) mask)
              h (if (>= (- w k) 64) 0 (unsigned-bit-shift-right p (- w k)))]
          (recur (+ i 1) (conj out (if (>= h 0) (str h) (str (+ (bigint h) (bit-shift-left (bigint 1) 64)))))))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
