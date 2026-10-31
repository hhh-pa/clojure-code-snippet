;; rt-scheduling: RMS Liu-Layland bound vs EDF utilization test (MOS 2.4.4).
;; Input: n; n lines C T. Output RMS YES/NO, EDF YES/NO.
;; 2^(1/n) via Newton iteration (uniform across languages). Pure Clojure.

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

(defn npow2inv [n]
  (loop [it 0 x 1.0]
    (if (>= it 200) x
      (let [p (loop [i 0 p 1.0] (if (>= i (- n 1)) p (recur (+ i 1) (* p x))))
            f (- (* p x) 2.0)
            d (* (double n) p)]
        (recur (+ it 1) (- x (/ f d)))))))

(defn solve [toks]
  (let [n (read-int (nth toks 0))
        u (loop [i 0 acc 0.0]
            (if (>= i n) acc
              (recur (+ i 1) (+ acc (/ (double (read-int (nth toks (+ 1 (* i 2)))))
                                       (double (read-int (nth toks (+ 2 (* i 2))))))))))
        bound (* (double n) (- (npow2inv n) 1.0))]
    [(if (<= u bound) "RMS YES" "RMS NO")
     (if (<= u 1.0) "EDF YES" "EDF NO")]))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
