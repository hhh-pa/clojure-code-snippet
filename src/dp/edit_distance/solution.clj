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
  (when (and (string? tok) (pos? (count tok)))
    (if (<= (count tok) 18)
      (if (= (nth tok 0) \+)
        (when (and (> (count tok) 1) (not= (nth tok 1) \-) (not= (nth tok 1) \+))
          (parse-long (subs tok 1)))
        (parse-long tok))
      (let [cs (seq tok)
            head (first cs)
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
  "Levenshtein gap between strings a and b with one mutable row.
  Time O(mn) and space O(min(m, n)) by folding over the longer string."
  [a b]
  (let [swap? (> (count a) (count b))
        s (if swap? b a)
        t (if swap? a b)
        m (count s)
        n (count t)]
    (if (zero? m)
      n
      (let [dp (transient (vec (range (inc m))))]
        (loop [i0 0]
          (if (= i0 n)
            (nth dp m)
            (let [bi (nth t i0)
                  i1 (inc i0)]
              (assoc! dp 0 i1)
              (loop [j 1 diag i0 left i1]
                (when (<= j m)
                  (let [up (nth dp j)
                        nl (if (= (nth s (dec j)) bi)
                             diag
                             (inc (let [m1 (if (< up left) up left)]
                                    (if (< m1 diag) m1 diag))))]
                    (assoc! dp j nl)
                    (recur (inc j) up nl))))
              (recur i1))))))))

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
