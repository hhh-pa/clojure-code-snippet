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

(defn lower-bound
  "First index in sorted tails holding value at least x, binary scan."
  [tails x]
  (loop [l 0
         r (count tails)]
    (if (< l r)
      (let [mid (quot (+' l r) 2)]
        (if (< (nth tails mid) x)
          (recur (inc mid) r)
          (recur l mid)))
      l)))

(defn lis-length
  "Length of longest rising run with patience piles and binary scan.
  Time O(n log n) and space O(n) for piles."
  [xs]
  (loop [tails []
         rest-xs (seq xs)]
    (if (nil? rest-xs)
      (count tails)
      (let [x (first rest-xs)
            idx (lower-bound tails x)]
        (recur (if (= idx (count tails))
                 (conj tails x)
                 (assoc tails idx x))
               (next rest-xs))))))

(defn complain
  "Print message to stderr and return nil, exit stays zero as host exit is out of reach."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn -main
  "Read length plus run from stdin and print rising length, quiet on blank input."
  [& _args]
  (let [data (slurp *in*)
        toks (tokenize data)]
    (when (seq toks)
      (let [nums (parse-all toks)]
        (cond
          (nil? nums)
          (complain "invalid input: expected integers only")

          (empty? nums)
          (complain "invalid input: expected length first")

          :else
          (let [n (first nums)]
            (cond
              (< n 0)
              (complain "invalid input: length must be zero or more")

              (< (count nums) (+' 1 n))
              (complain "invalid input: not enough run values")

              :else
              (let [xs (->> nums (drop 1) (take n) vec)]
                (println (str (lis-length xs)))))))))))

(-main)
