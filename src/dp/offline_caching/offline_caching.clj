;; offline-caching: Belady furthest-in-future optimal replacement
;;
;; Simulates the optimal offline pager: on a miss with a full cache
;; the resident page used furthest in the future (never-again first,
;; ties by smallest id) is evicted. Emits the fault count.
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
    (let [k (nth nums 0) m (nth nums 1)
          rq (mapv #(nth nums (+ 2 %)) (range m))
          inf 1000000000]
      (if (<= k 0)
        (println m)
        (let [qs (atom (reduce (fn [acc i] (update acc (nth rq i) (fnil conj []) i)) {} (range m)))
              qi (atom (zipmap (keys @qs) (repeat 0)))
              cache (atom #{})
              faults (atom 0)]
          (doseq [i (range m)]
            (let [p (nth rq i)]
              (swap! qi update p inc)
              (when (not (contains? @cache p))
                (swap! faults inc)
                (if (< (count @cache) k)
                  (swap! cache conj p)
                  (let [nu (fn [c] (let [lst (get @qs c) idx (get @qi c 0)]
                                              (if (< idx (count lst)) (nth lst idx) inf)))
                        best (reduce (fn [b c] (let [x (nu c) y (nu b)]
                                                 (if (or (> x y) (and (= x y) (< c b))) c b)))
                                     (first (sort @cache)) (rest (sort @cache)))]
                    (swap! cache disj best)
                    (swap! cache conj p))))))
          (println @faults))))))
