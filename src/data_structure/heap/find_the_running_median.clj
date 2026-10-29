(require 'clojure.string)

;; Optimal bounds: two balanced multisets keep every insert and rebalance at
;; O(log n), for O(n log n) total time and O(n) space, matching the sorting
;; lower bound for online medians.

(def digit-values
  "Map from each digit char to its numeric value."
  (zipmap "0123456789" (range)))

(def max-long
  "Largest signed 64-bit value; tokens beyond long range are invalid."
  9223372036854775807)

(def min-long
  "Smallest signed 64-bit value."
  -9223372036854775808)

(defn fit-long
  "Returns n when it fits in a signed 64-bit long, else nil."
  [n]
  (when (<= min-long n max-long) n))


(def whitespace-chars
  "Chars treated as token separators on input."
  #{\space \tab \newline \return})

(defn tokenize
  "Split s into whitespace separated tokens in O(n) time."
  [s]
  (loop [cs (seq s) cur [] toks []]
    (if (nil? cs)
      (if (seq cur) (conj toks (apply str cur)) toks)
      (let [ch (first cs)]
        (if (contains? whitespace-chars ch)
          (recur (next cs) [] (if (seq cur) (conj toks (apply str cur)) toks))
          (recur (next cs) (conj cur ch) toks))))))

(defn parse-num
  "Parse tok as an integer, or nil when it is not one."
  [tok]
  (when (and (string? tok) (seq tok))
    (let [cs (seq tok)
          neg? (= (first cs) \-)
          ds (if neg? (next cs) cs)]
      (when (and (seq ds) (every? digit-values ds))
        (let [v (reduce (fn [acc ch] (+' (*' acc 10) (digit-values ch))) 0 ds)]
          (fit-long (if neg? (-' v) v)))))))

(defn fail
  "Report invalid input on stderr and return normally.
   Note: the process still exits 0 since a nonzero status needs host calls."
  [msg]
  (binding [*out* *err*]
    (println msg)))

(defn emit-lines
  "Print each line with a trailing newline, or nothing when there are none."
  [lines]
  (when (seq lines)
    (print (str (clojure.string/join "\n" lines) "\n"))))

(defn counts-add
  "Add one copy of v to the value count map."
  [m v]
  (assoc m v (inc (get m v 0))))

(defn counts-take
  "Remove one copy of v from the value count map."
  [m v]
  (let [c (get m v 0)]
    (if (<= c 1)
      (dissoc m v)
      (assoc m v (dec c)))))

(defn counts-min
  "Smallest key present, in O(log n)."
  [m]
  (key (first m)))

(defn counts-max
  "Largest key present, in O(log n)."
  [m]
  (key (first (rseq m))))

(defn format-median
  "Render the median of a and b with exactly one decimal place.
   A mean of two integers is whole or half, so appending .0 or .5 is exact."
  [a b odd?]
  (if odd?
    (str a ".0")
    (let [s (+' a b)
          q (quot s 2)]
      (cond
        (even? s) (str q ".0")
        (and (neg? s) (zero? q)) "-0.5"
        :else (str q ".5")))))

(defn running-medians
  "Median after each prefix of vals, one formatted line per value."
  [vals]
  (loop [vs vals lo (sorted-map) lo-n 0 hi (sorted-map) hi-n 0 out []]
    (if (empty? vs)
      out
      (let [v (first vs)
            [lo lo-n hi hi-n] (if (or (zero? lo-n) (<= v (counts-max lo)))
                               [(counts-add lo v) (inc lo-n) hi hi-n]
                               [lo lo-n (counts-add hi v) (inc hi-n)])
            [lo lo-n hi hi-n] (cond
                                (> lo-n (inc hi-n))
                                (let [m (counts-max lo)]
                                  [(counts-take lo m) (dec lo-n)
                                   (counts-add hi m) (inc hi-n)])
                                (> hi-n lo-n)
                                (let [m (counts-min hi)]
                                  [(counts-add lo m) (inc lo-n)
                                   (counts-take hi m) (dec hi-n)])
                                :else [lo lo-n hi hi-n])
            line (if (> lo-n hi-n)
                   (format-median (counts-max lo) (counts-max lo) true)
                   (format-median (counts-max lo) (counts-min hi) false))]
        (recur (rest vs) lo lo-n hi hi-n (conj out line))))))

(defn -main
  "Read n values and print the running median after each one."
  [input]
  (let [toks (tokenize input)
        n (parse-num (first toks))]
    (if (or (nil? n) (neg? n) (< (count toks) (inc' n)))
      (fail "invalid input: expected a count n, then n values")
      (let [vals (mapv parse-num (take n (rest toks)))]
        (if (some nil? vals)
          (fail "invalid input: expected a count n, then n values")
          (emit-lines (running-medians vals)))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
