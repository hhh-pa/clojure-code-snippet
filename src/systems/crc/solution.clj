;; crc: polynomial modulo-2 division remainder (Tanenbaum networks).
;; Input: generator binary string; message binary string.
;; Output: remainder of length len(G)-1. Pure Clojure.

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
  (let [G (nth toks 0) M (nth toks 1)
        r (- (count G) 1)
        d0 (vec (concat (mapv (fn [c] (if (= c \1) 1 0)) M) (repeat r 0)))
        d (loop [i 0 dd d0]
            (if (>= i (count M)) dd
              (if (= (nth dd i) 1)
                (recur (+ i 1)
                       (loop [j 0 e dd]
                         (if (>= j (count G)) e
                           (recur (+ j 1) (if (= (nth G j) \1)
                                            (assoc e (+ i j) (bit-xor (nth e (+ i j)) 1)) e)))))
                (recur (+ i 1) dd))))]
    (apply str (map str (subvec d (count M))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (println (solve toks))))
