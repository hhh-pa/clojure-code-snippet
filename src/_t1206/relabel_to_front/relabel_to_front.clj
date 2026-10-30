;; relabel-to-front: push-relabel maximum flow
;;
;; Relabel-to-front selection over a residual matrix with
;; persistent current-edge pointers. Emits the maximum flow value.
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
      (let [h (atom (assoc (vec (repeat n 0)) s n))
            e (atom (vec (repeat n 0)))]
        (doseq [v (range n)]
          (when (pos? (nth (nth @res s) v))
            (let [c (nth (nth @res s) v)]
              (swap! e assoc v c)
              (swap! e assoc s (- (nth @e s) c))
              (swap! res assoc-in [v s] (+ (nth (nth @res v) s) c))
              (swap! res assoc-in [s v] 0))))
        (let [L (atom (filterv #(and (not= % s) (not= % t)) (range n)))
              cur (atom (vec (repeat n 0)))]
          (loop [p 0]
            (when (< p (count @L))
              (let [u (nth @L p) old (nth @h u)]
                (loop []
                  (when (pos? (nth @e u))
                    (let [v (nth @cur u)]
                      (cond (= v n)
                            (do (swap! h assoc u (inc (reduce min (map #(nth @h %) (filter #(pos? (nth (nth @res u) %)) (range n))))))
                                (swap! cur assoc u 0))
                            (and (pos? (nth (nth @res u) v)) (= (nth @h u) (inc (nth @h v))))
                            (let [d (min (nth @e u) (nth (nth @res u) v))]
                              (swap! res assoc-in [u v] (- (nth (nth @res u) v) d))
                              (swap! res assoc-in [v u] (+ (nth (nth @res v) u) d))
                              (swap! e assoc u (- (nth @e u) d))
                              (swap! e assoc v (+ (nth @e v) d)))
                            :else (swap! cur assoc u (inc v))))
                    (recur)))
                (if (> (nth @h u) old)
                  (do (reset! L (vec (cons u (concat (subvec @L 0 p) (subvec @L (inc p))))))
                      (recur 0))
                  (recur (inc p))))))
          (println (nth @e t)))))))
