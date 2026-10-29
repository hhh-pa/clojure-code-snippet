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

(defn count-ways
  "Count combinations for amount using coins, outer coin inner sum scan.
  Time O(mn) and space O(n) with transient row, treating row ops as unit cost."
  [amount coins]
  (let [row (transient (vec (cons 1 (repeat amount 0))))]
    (doseq [c coins]
      (loop [j c]
        (when (<= j amount)
          (assoc! row j (+' (nth row j) (nth row (- j c))))
          (recur (inc j)))))
    (nth (persistent! row) amount)))

(defn complain
  "Print message to stderr and return nil, exit stays zero as host exit is out of reach."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn -main
  "Read amount coins from stdin and print count, quiet on blank input."
  [& _args]
  (let [data (slurp *in*)
        toks (tokenize data)]
    (when (seq toks)
      (let [nums (parse-all toks)]
        (cond
          (nil? nums)
          (complain "invalid input: expected integers for amount and coins")

          (< (count nums) 2)
          (complain "invalid input: expected amount followed by coin count")

          :else
          (let [amount (nth nums 0)
                m (nth nums 1)]
            (cond
              (or (< amount 0) (< m 0))
              (complain "invalid input: amount and coin count must be zero or more")

              (< (count nums) (+' 2 m))
              (complain "invalid input: not enough coin values")

              :else
              (let [coins (->> nums (drop 2) (take m) vec)]
                (if (some (fn [c] (<= c 0)) coins)
                  (complain "invalid input: coin values must be one or more")
                  (print (str (count-ways amount coins))))))))))))

(-main)
