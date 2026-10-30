;; csma-cd-backoff: binary exponential backoff (truncated at 2^10).
;; Input: q; each: k d1..dk (draws). After i-th collision
;; window = 2^min(i,10); slots = d_i mod window. Output per line:
;; "<total> <win1> ... <wink>" (total only when k=0). Pure Clojure.

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
  (let [q (read-int (nth toks 0))]
    (loop [i 0 pos 1 out []]
      (if (>= i q) out
        (let [k (read-int (nth toks pos))
              [tot wins] (loop [j 1 acc 0 ws []]
                           (if (> j k) [acc ws]
                             (let [d (read-int (nth toks (+ pos j)))
                                   e (if (< j 10) j 10)
                                   win (bit-shift-left 1 e)]
                               (recur (+ j 1) (+ acc (mod (if (neg? d) (- d) d) win)) (conj ws win)))))]
          (recur (+ i 1) (+ pos 1 k)
                 (conj out (str tot (apply str (map (fn [w] (str " " w)) wins))))))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
