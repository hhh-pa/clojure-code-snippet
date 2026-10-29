(def digit-val
  "Map from decimal char to numeric value for pure parsing."
  (zipmap "0123456789" (range)))

(defn tokenize
  "Split string on whitespace into a vector of token strings using pure reduce."
  [s]
  (let [sp (int \space)
        step (fn [[toks cur] c]
               (if (<= (int c) sp)
                 (if (empty? cur)
                   [toks cur]
                   [(conj toks (apply str cur)) []])
                 [toks (conj cur c)]))
        found (reduce step [[] []] (or (seq s) []))
        toks (first found)
        cur (second found)]
    (if (empty? cur)
      toks
      (conj toks (apply str cur)))))

(defn parse-num
  "Parse decimal token to integer value or nil when shape is bad."
  [tok]
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
              (if neg (- 0 val) val))))))))

(defn parse-all
  "Parse every token with parse-num, giving nil when any token is bad."
  [toks]
  (reduce (fn [acc t]
            (let [v (parse-num t)]
              (if (nil? v)
                (reduced nil)
                (conj acc v))))
          [] toks))

(defn unbounded-one
  "Best fill for cap with reuse of vals where value equals weight, scanning upward.
  Time O(ns) and space O(s) with transient row, treating row ops as unit cost."
  [cap vals]
  (let [row (transient (vec (repeat (inc cap) 0)))]
    (loop [c 0]
      (when (<= c cap)
        (doseq [w vals]
          (when (<= w c)
            (let [nv (+' (nth row (- c w)) w)]
              (when (> nv (nth row c))
                (assoc! row c nv)))))
        (recur (inc c))))
    (nth (persistent! row) cap)))

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
        (let [vals (->> nums (drop (+ p 2)) (take n) vec)]
          (if (some (fn [w] (< w 0)) vals)
            (complain "invalid input: weights must be zero or more")
            (do
              (println (str (unbounded-one s vals)))
              (solve-cases nums (+ p 2 n) (dec left)))))))))

(defn -main
  "Read test cases from stdin and print best fills, quiet on blank input."
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
