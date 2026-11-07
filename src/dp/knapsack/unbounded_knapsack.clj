(def digit-val
  "Map from decimal char to numeric value for pure parsing."
  (zipmap "0123456789" (range)))

(defn tokenize
  "Split string on whitespace into a vector of token strings."
  [s]
  (if (nil? s)
    []
    (let [v (clojure.string/split s #"[\x00-\x20]+")
          n (count v)
          start (if (and (pos? n) (clojure.string/blank? (nth v 0))) 1 0)
          end (if (and (> n start) (clojure.string/blank? (nth v (dec n)))) (dec n) n)]
      (subvec v start end))))

(defn parse-num
  "Parse decimal token to integer value or nil when shape is bad."
  [tok]
  (if (and (string? tok) (re-matches #"[+-]?[0-9]{1,18}" tok))
    (parse-long tok)
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
  (let [n (count toks)]
    (loop [i 0 acc (transient [])]
      (if (= i n)
        (persistent! acc)
        (let [v (parse-num (nth toks i))]
          (if (nil? v)
            nil
            (recur (inc i) (conj! acc v))))))))

(defn unbounded-one
  "Best fill for cap with reuse of vals where value equals weight, scanning upward.
  Time O(ns) and space O(s) with transient row."
  [cap vals]
  (let [row (transient (vec (repeat (inc cap) 0)))]
    (doseq [w (distinct vals)]
      (when (and (pos? w) (<= w cap))
        (loop [c w]
          (when (<= c cap)
            (let [nv (+' (nth row (- c w)) w)]
              (when (> nv (nth row c))
                (assoc! row c nv)))
            (recur (inc c))))))
    (nth row cap)))

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
    (let [n (nth nums p nil)
          s (nth nums (inc p) nil)]
      (cond
        (or (nil? n) (nil? s))
        (complain "invalid input: missing case header")

        (or (< n 0) (< s 0))
        (complain "invalid input: kind count and cap must be zero or more")

        (< (count nums) (+' p 2 n))
        (complain "invalid input: not enough weight values")

        :else
        (let [vals (subvec nums (+ p 2) (+ p 2 n))]
          (if (some (fn [w] (< w 0)) vals)
            (complain "invalid input: weights must be zero or more")
            (do
              (println (str (unbounded-one s vals)))
              (recur nums (+ p 2 n) (dec left)))))))))

(defn -main
  "Read test cases from stdin and print best fills, quiet on blank input."
  [& _args]
  (let [data (slurp *in*)
        toks (tokenize data)]
    (when (pos? (count toks))
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
