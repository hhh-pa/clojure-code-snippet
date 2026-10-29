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

(defn lcs-scores
  "Row of longest match lengths for x against each prefix of y.
  Time O(mn) and space O(n) with two rows, treating row ops as unit cost."
  [x y]
  (let [m (count y)
        base (vec (repeat (inc m) 0))]
    (loop [prev base
           xs (seq x)]
      (if (nil? xs)
        prev
        (let [xi (first xs)
              cur (transient base)]
          (loop [j 1]
            (when (<= j m)
              (if (= xi (nth y (dec j)))
                (assoc! cur j (inc (nth prev (dec j))))
                (assoc! cur j (max (nth prev j) (nth cur (dec j)))))
              (recur (inc j))))
          (recur (persistent! cur) (next xs)))))))

(defn in-vec
  "True when v equals some entry of coll, pure linear scan."
  [coll v]
  (some? (some (fn [e] (= e v)) coll)))

(defn hirschberg
  "Longest shared run of x and y with linear extra rows and first split.
  Time O(mn) and extra space O(n), split choice mirrors full table backtrack."
  [x y]
  (cond
    (or (empty? x) (empty? y))
    []

    (= 1 (count x))
    (let [v (first x)]
      (if (in-vec y v) [v] []))

    (= 1 (count y))
    (let [v (first y)]
      (if (in-vec x v) [v] []))

    :else
    (let [mid (quot (count x) 2)
          x1 (subvec x 0 mid)
          x2 (subvec x mid)
          fwd (lcs-scores x1 y)
          back (lcs-scores (vec (reverse x2)) (vec (reverse y)))
          m (count y)
          best-k (loop [k 0 best-val -1 best-k 0]
                   (if (> k m)
                     best-k
                     (let [v (+' (nth fwd k) (nth back (- m k)))]
                       (if (> v best-val)
                         (recur (inc k) v k)
                         (recur (inc k) best-val best-k)))))]
      (into (hirschberg x1 (subvec y 0 best-k))
            (hirschberg x2 (subvec y best-k))))))

(defn complain
  "Print message to stderr and return nil, exit stays zero as host exit is out of reach."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn -main
  "Read two int runs from stdin and print shared run, quiet on blank input."
  [& _args]
  (let [data (slurp *in*)
        toks (tokenize data)]
    (when (seq toks)
      (let [nums (parse-all toks)]
        (cond
          (nil? nums)
          (complain "invalid input: expected integers only")

          (< (count nums) 2)
          (complain "invalid input: expected two lengths first")

          :else
          (let [n (nth nums 0)
                m (nth nums 1)]
            (cond
              (or (< n 0) (< m 0))
              (complain "invalid input: lengths must be zero or more")

              (< (count nums) (+' 2 n m))
              (complain "invalid input: not enough run values")

              :else
              (let [a (->> nums (drop 2) (take n) vec)
                    b (->> nums (drop (+ 2 n)) (take m) vec)
                    ans (hirschberg b a)
                    line (apply str (interpose " " (map str ans)))]
                (println line)))))))))

(-main)
