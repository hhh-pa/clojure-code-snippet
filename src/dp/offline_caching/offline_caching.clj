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
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

(def min-long
  "Most negative long value: 1 shifted into the sign bit."
  (bit-shift-left 1 63))

(def max-long
  "Largest long value: every bit below the sign bit set."
  (bit-not min-long))

(defn parse-token
  "Parse tok as a decimal long integer with optional sign. Returns
   nil when tok is malformed or out of long range: the sign and ASCII
   digits are matched first, then parse-long reads the value."
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))

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
                                     (first @cache) @cache)]
                    (swap! cache disj best)
                    (swap! cache conj p))))))
          (println @faults))))))
