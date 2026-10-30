;; dag-shortest-paths: shortest and longest paths in a DAG
;;
;; Kahn topological order then one relaxation pass for shortest
;; paths and one for longest (critical path). Emits both rows, with
;; INF markers for the unreachable.
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
          ind (atom (vec (repeat n 0)))
          _ (doseq [i (range m)] (let [v (nth nums (+ 4 (* i 3)))] (swap! ind assoc v (inc (nth @ind v)))))
          inf (bit-shift-left 1 60)
          topo (loop [h (vec (sort (filter #(zero? (nth @ind %)) (range n)))) acc []]
                 (if (empty? h) acc
                   (let [u (first h)]
                     (doseq [[v _] (nth adj u)]
                       (swap! ind assoc v (dec (nth @ind v))))
                     (recur (vec (sort (concat (rest h) (filter (fn [v] (and (zero? (nth @ind v)) (not (boolean (some #(= % v) acc))) (not= v u))) (map first (nth adj u)))))) (conj acc u)))))
          ds (atom (assoc (vec (repeat n inf)) s 0))
          dl (atom (assoc (vec (repeat n (- inf))) s 0))]
      (doseq [u topo [v w] (nth adj u)]
        (when (< (+ (nth @ds u) w) (nth @ds v))
          (swap! ds assoc v (+ (nth @ds u) w)))
        (when (and (> (nth @dl u) (quot (- inf) 2)) (> (+ (nth @dl u) w) (nth @dl v)))
          (swap! dl assoc v (+ (nth @dl u) w))))
      (println (clojure.string/join " " (map #(if (>= % (quot inf 2)) "INF" (str %)) @ds)))
      (println (clojure.string/join " " (map #(if (<= % (quot (- inf) 2)) "-INF" (str %)) @dl))))))
