;; spfa: shortest path faster algorithm
;;
;; Queue-driven Bellman-Ford relaxation with parent-pointer cycle
;; checks plus a relaxation-count guard. Emits distances or a negative
;; cycle marker.
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
          inf (bit-shift-left 1 60)
          dist (atom (assoc (vec (repeat n inf)) s 0))
          inq (atom (assoc (vec (repeat n false)) s true))
          cnt (atom (vec (repeat n 0)))
          par (atom (vec (repeat n -1)))
          cyc (fn [v] (loop [x v i 0]
                         (cond (> i n) true
                               :else (let [nx (nth @par x)]
                                       (cond (= nx -1) false (= nx v) true :else (recur nx (inc i)))))))]
      (loop [q [s]]
        (if (empty? q)
          (println (clojure.string/join " " (map #(if (>= % (quot inf 2)) "INF" (str %)) @dist)))
          (let [u (first q)]
            (swap! inq assoc u false)
            (let [neg (atom false) nq (atom (vec (rest q)))]
              (doseq [[v w] (nth adj u)]
                (when (and (not @neg) (< (+ (nth @dist u) w) (nth @dist v)))
                  (swap! dist assoc v (+ (nth @dist u) w))
                  (swap! par assoc v u)
                  (if (cyc v)
                    (reset! neg true)
                    (when (not (nth @inq v))
                      (swap! nq conj v)
                      (swap! inq assoc v true)
                      (swap! cnt assoc v (inc (nth @cnt v)))
                      (when (>= (nth @cnt v) n) (reset! neg true))))))
              (if @neg
                (println "NEGATIVE CYCLE")
                (recur @nq)))))))))
