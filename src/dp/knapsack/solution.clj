(def digit-val
  "Map from decimal char to numeric value for pure parsing."
  (zipmap "0123456789" (range)))

(defn tokenize
  "Split string on whitespace into a vector of token strings using pure reduce."
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[\x00-\x20]+"))))

(defn parse-num
  "Parse decimal token to integer value or nil when shape is bad."
  [tok]
  (if-let [v (when (string? tok) (parse-long tok))]
    v
    (let [cs (seq tok)]
      (when (seq cs)
        (let [head (first cs)
              neg (= head \-)
              plus (= head \+)
              rest-cs (if (or neg plus) (next cs) cs)]
          (when (seq rest-cs)
            (let [val (reduce (fn [acc c]
                                (let [d (get digit-val c)]
                                  (if (nil? d)
                                    (reduced nil)
                                    (+' (*' acc 10) d))))
                              0 rest-cs)]
              (when (some? val)
                (if neg (- 0 val) val)))))))))

(defn parse-all
  "Parse every token with parse-num, giving nil when any token is bad."
  [toks]
  (reduce (fn [acc t]
            (let [v (parse-num t)]
              (if (nil? v)
                (reduced nil)
                (conj acc v))))
          [] toks))

(defn knapsack-one
  "Best value for cap with items read directly from nums starting at start-idx."
  [cap nums start-idx n]
  (let [row (transient (vec (repeat (inc cap) 0)))
        end-idx (+ start-idx (* 2 n))]
    (loop [idx start-idx]
      (if (>= idx end-idx)
        (nth (persistent! row) cap)
        (let [w (nth nums idx)
              v (nth nums (inc idx))]
          (when (and (<= w cap) (> v 0))
            (loop [c cap]
              (when (>= c w)
                (let [nv (+' (nth row (- c w)) v)]
                  (when (> nv (nth row c))
                    (assoc! row c nv)))
                (recur (dec c)))))
          (recur (+ idx 2)))))))

(defn complain
  "Print message to stderr and return nil, exit stays zero as host exit is out of reach."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn solve-cases
  "Walk nums from index p for left cases, printing each answer."
  [nums p left]
  (when (> left 0)
    (let [s (nth nums p nil)
          n (nth nums (inc p) nil)]
      (cond
        (or (nil? s) (nil? n))
        (complain "invalid input: missing case header")

        (or (< s 0) (< n 0))
        (complain "invalid input: cap and item count must be zero or more")

        (< (count nums) (+' p 2 (*' 2 n)))
        (complain "invalid input: not enough item pairs")

        :else
        (let [start-idx (+ p 2)
              end-idx (+ start-idx (* 2 n))
              bad-w? (loop [idx start-idx]
                       (if (>= idx end-idx)
                         false
                         (if (< (nth nums idx) 0)
                           true
                           (recur (+ idx 2)))))]
          (if bad-w?
            (complain "invalid input: weights must be zero or more")
            (do
              (println (str (knapsack-one s nums start-idx n)))
              (solve-cases nums end-idx (dec left)))))))))

(defn -main
  "Read test cases from stdin and print best values, quiet on blank input."
  [& _args]
  (let [data (slurp *in*)
        toks (tokenize data)]
    (when (seq toks)
      (let [nums (parse-all toks)]
        (cond
          (nil? nums)
          (complain "invalid input: expected integers only")

          (empty? nums)
          (complain "invalid input: expected case count")

          :else
          (let [t (first nums)]
            (if (< t 0)
              (complain "invalid input: case count must be zero or more")
              (solve-cases nums 1 t))))))))

(-main)
