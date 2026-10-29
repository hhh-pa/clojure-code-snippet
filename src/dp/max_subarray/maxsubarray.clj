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

(defn max-sums
  "Best block sum and best loose sum for non blank run in one scan.
  Time O(n) and extra space O(1) with kadane scan plus positive fold."
  [vals]
  (let [head (first vals)]
    (loop [rest-vs (next vals)
           cur head
           best head
           max-elem head
           pos-sum (if (> head 0) head 0)]
      (if (nil? rest-vs)
        [best (if (> pos-sum 0) pos-sum max-elem)]
        (let [v (first rest-vs)
              ncur (max v (+' cur v))
              nbest (max best ncur)
              nelem (max max-elem v)
              npos (if (> v 0) (+' pos-sum v) pos-sum)]
          (recur (next rest-vs) ncur nbest nelem npos))))))

(defn complain
  "Print message to stderr and return nil, exit stays zero as host exit is out of reach."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn solve-cases
  "Walk nums from index p for left cases, printing each answer pair."
  [nums p left]
  (when (> left 0)
    (let [n (nth nums p nil)]
      (cond
        (nil? n)
        (complain "invalid input: missing case length")

        (< n 1)
        (complain "invalid input: case length must be one or more")

        (< (count nums) (+' p 1 n))
        (complain "invalid input: not enough case values")

        :else
        (let [vals (->> nums (drop (+ p 1)) (take n) vec)
              pair (max-sums vals)]
          (println (str (first pair) " " (second pair)))
          (solve-cases nums (+ p 1 n) (dec left)))))))

(defn -main
  "Read test cases from stdin and print best pairs, quiet on blank input."
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
