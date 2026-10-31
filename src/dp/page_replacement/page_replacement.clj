;; page-replacement: FIFO, LRU and randomized marking
;;
;; Simulates FIFO and LRU plus the randomized marking algorithm with
;; a fixed-seed LCG so the eviction choices are deterministic. Emits
;; the three fault counts.
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
  (when (>= (count nums) 2)
    (let [f (nth nums 0) m (nth nums 1)
          rq (mapv #(nth nums (+ 2 %)) (range m))
          rng (atom 42)
          rnd (fn [] (swap! rng (fn [x] (mod (+ (* 1103515245 x) 12345) (bit-shift-left 1 31)))) @rng)]
      (let [ff (loop [rs rq q [] s #{} acc 0]
                 (if (empty? rs) acc
                   (let [p (first rs)]
                     (cond (<= f 0) (recur (rest rs) q s (inc acc))
                           (contains? s p) (recur (rest rs) q s acc)
                           :else (let [[q2 s2] (if (>= (count q) f) [(vec (rest q)) (disj s (first q))] [q s])]
                                   (recur (rest rs) (conj q2 p) (conj s2 p) (inc acc)))))))
            lru-res (loop [rs rq lru [] acc 0]
                      (if (empty? rs) acc
                        (let [p (first rs)]
                          (cond (<= f 0) (recur (rest rs) lru (inc acc))
                                (boolean (some #(= % p) lru))
                                (recur (rest rs) (conj (vec (remove #(= % p) lru)) p) acc)
                                :else (let [lru2 (if (>= (count lru) f) (vec (rest lru)) lru)]
                                        (recur (rest rs) (conj lru2 p) (inc acc)))))))
            mf (loop [rs rq cache #{} marked #{} acc 0]
                 (if (empty? rs) acc
                   (let [p (first rs)]
                     (cond (<= f 0) (recur (rest rs) cache marked (inc acc))
                           (contains? cache p) (recur (rest rs) cache (conj marked p) acc)
                           (< (count cache) f) (recur (rest rs) (conj cache p) (conj marked p) (inc acc))
                           :else (let [mk (if (>= (count marked) (count cache)) #{} marked)
                                       cands (vec (sort (remove #(contains? mk %) cache)))
                                       ev (nth cands (mod (rnd) (count cands)))]
                                   (recur (rest rs) (conj (disj cache ev) p) (conj (disj mk ev) p) (inc acc)))))))]
        (println ff)
        (println lru-res)
        (println mf)))))
