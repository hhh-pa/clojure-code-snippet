;; edmonds-karp: max flow by shortest augmenting paths
;;
;; Ford-Fulkerson with BFS shortest augmenting paths over a
;; residual matrix. Emits the maximum flow value.
;; Reads stdin tokens, writes the answer to stdout.

(def digit-values
  "Digit character to numeric value."
  (zipmap "0123456789" (range)))

(def separators
  "Whitespace characters that separate input tokens."
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize
  "Split s on whitespace into a vector of token strings."
  [s]
  (let [done (reduce (fn [acc ch]
                       (if (contains? separators ch)
                         (if (empty? (:current acc))
                           acc
                           {:tokens (conj (:tokens acc) (apply str (:current acc)))
                             :current []})
                         (update acc :current conj ch)))
                     {:tokens [] :current []}
                     s)]
    (if (empty? (:current done))
      (:tokens done)
      (conj (:tokens done) (apply str (:current done))))))

(def min-long
  "Most negative long value: 1 shifted into the sign bit."
  (bit-shift-left 1 63))

(def max-long
  "Largest long value: every bit below the sign bit set."
  (bit-not min-long))

(defn parse-token
  "Parse tok as a decimal long integer with optional sign. Returns
   nil when tok is malformed or out of long range; the bounds check
   runs before each multiply-and-add step so no step can overflow."
  [tok]
  (let [chs (seq tok)]
    (when (seq chs)
      (let [head (first chs)
            neg? (= head \-)
            rest-chs (if (or neg? (= head \+)) (rest chs) chs)]
        (when (seq rest-chs)
          (if neg?
            (reduce (fn [acc ch]
                      (when (some? acc)
                        (let [d (get digit-values ch)]
                          (when (and (some? d)
                                     (>= acc (quot (+ min-long d) 10)))
                            (- (* acc 10) d)))))
                    0 rest-chs)
            (reduce (fn [acc ch]
                      (when (some? acc)
                        (let [d (get digit-values ch)]
                          (when (and (some? d)
                                     (<= acc (quot (- max-long d) 10)))
                            (+ (* acc 10) d)))))
                    0 rest-chs)))))))

(defn read-longs
  "Read all of stdin as a vector of longs."
  []
  (mapv parse-token (tokenize (slurp *in*))))


(let [nums (read-longs)]
  (when (>= (count nums) 4)
    (let [n (nth nums 0) m (nth nums 1) s (nth nums 2) t (nth nums 3)
          res (atom (mapv (fn [_] (mapv (fn [_] 0) (range n))) (range n)))]
      (doseq [i (range m)]
        (let [u (nth nums (+ 4 (* i 3))) v (nth nums (+ 5 (* i 3))) c (nth nums (+ 6 (* i 3)))]
          (swap! res assoc-in [u v] (+ (nth (nth @res u) v) c))))
      (loop [flow 0]
        (let [par (atom (assoc (vec (repeat n -1)) s s))]
          (loop [q [s]]
            (when (and (seq q) (= (nth @par t) -1))
              (let [u (first q)
                    new-vs (filterv (fn [v] (and (= (nth @par v) -1) (pos? (nth (nth @res u) v)))) (range n))]
                (doseq [v new-vs] (swap! par assoc v u))
                (recur (vec (concat (rest q) new-vs))))))
          (if (= (nth @par t) -1)
            (println flow)
            (let [b (loop [v t acc (bit-shift-left 1 60)]
                      (if (= v s) acc (recur (nth @par v) (min acc (nth (nth @res (nth @par v)) v)))))]
              (loop [v t]
                (when (not= v s)
                  (let [u (nth @par v)]
                    (swap! res assoc-in [u v] (- (nth (nth @res u) v) b))
                    (swap! res assoc-in [v u] (+ (nth (nth @res v) u) b))
                    (recur u))))
              (recur (+ flow b)))))))))
