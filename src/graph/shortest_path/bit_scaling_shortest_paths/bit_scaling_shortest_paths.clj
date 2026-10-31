;; bit-scaling-shortest-paths: integer-weight SSSP by bucket scaling
;;
;; Bucketed Dial shortest paths for non-negative integer weights:
;; monotone bucket minima extract settled vertices, the kernel of
;; Gabow bit-scaling phases. Emits distances with INF markers.
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
  (when (>= (count nums) 3)
    (let [n (nth nums 0) m (nth nums 1) s (nth nums 2)
          adj (reduce (fn [acc i] (let [u (nth nums (+ 3 (* i 3))) v (nth nums (+ 4 (* i 3))) w (nth nums (+ 5 (* i 3)))]
                                       (update acc u conj [v w])))
                       (mapv (fn [_] []) (range n)) (range m))
          cmax (reduce max 0 (map #(nth % 1) (mapcat #(nth adj %) (range n))))
          inf (bit-shift-left 1 60)
          dist (atom (assoc (vec (repeat n inf)) s 0))
          maxd (max 1 (* (dec n) cmax))
          bk (atom (assoc (vec (repeat (inc maxd) [])) 0 [s]))
          done (atom (vec (repeat n false)))]
      (loop [idx 0 pending 1]
        (when (pos? pending)
          (let [i2 (loop [j idx] (if (or (> j maxd) (seq (nth @bk j))) j (recur (inc j))))]
            (when (<= i2 maxd)
              (let [u (peek (nth @bk i2))]
                (swap! bk assoc i2 (pop (nth @bk i2)))
                (if (nth @done u)
                  (recur i2 pending)
                  (do
                    (swap! done assoc u true)
                    (let [np (atom (dec pending))]
                      (doseq [[v w] (nth adj u)]
                        (when (< (+ (nth @dist u) w) (nth @dist v))
                          (swap! dist assoc v (+ (nth @dist u) w))
                          (swap! bk assoc (nth @dist v) (conj (nth @bk (nth @dist v)) v))
                          (swap! np inc)))
                      (recur i2 @np)))))))))
      (println (clojure.string/join " " (map #(if (>= % (quot inf 2)) "INF" (str %)) @dist))))))
