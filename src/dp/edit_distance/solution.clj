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

(defn edit-distance
  "Levenshtein gap between strings a and b with two short rows.
  Time O(mn) and space O(min) by folding over the longer string."
  [a b]
  (let [pair (if (<= (count a) (count b)) [a b] [b a])
        s (first pair)
        t (second pair)
        m (count s)
        s-vec (vec s)]
    (if (zero? m)
      (count t)
      (loop [prev (vec (range (inc m)))
             t-rest (seq t)
             i 1]
        (if (nil? t-rest)
          (nth prev m)
          (let [bi (first t-rest)
                cur (transient (vec (repeat (inc m) 0)))]
            (assoc! cur 0 i)
            (loop [j 1]
              (when (<= j m)
                (if (= (nth s-vec (dec j)) bi)
                  (assoc! cur j (nth prev (dec j)))
                  (assoc! cur j (inc (min (nth prev j)
                                          (nth cur (dec j))
                                          (nth prev (dec j))))))
                (recur (inc j))))
            (recur (persistent! cur) (next t-rest) (inc i))))))))

(defn complain
  "Print message to stderr and return nil, exit stays zero as host exit is out of reach."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn -main
  "Read count plus string pairs from stdin and print gaps, quiet on blank input."
  [& _args]
  (let [data (slurp *in*)
        toks (tokenize data)]
    (when (seq toks)
      (let [t (parse-num (first toks))]
        (cond
          (nil? t)
          (complain "invalid input: expected integer count first")

          (< t 0)
          (complain "invalid input: count must be zero or more")

          (< (count toks) (+' 1 (*' 2 t)))
          (complain "invalid input: not enough strings for count")

          :else
          (let [pairs (->> toks (drop 1) (take (* 2 t)) (partition 2))]
            (doseq [[a b] pairs]
              (println (str (edit-distance a b))))))))))

(-main)
