(require 'clojure.string)

;; Sparse Table for Static Range Minimum, Maximum, and GCD Queries (Gusfield Ch 8):
;; Preprocesses an array of length N in O(N log N) time and space, answering
;; range min/max queries in O(1) time and range GCD queries in O(log(max A)).

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
  "Report invalid input on stderr and return normally."
  [msg]
  (binding [*out* *err*]
    (println msg)))

(defn emit-lines
  "Print each line with a trailing newline, or nothing when there are none."
  [lines]
  (when (seq lines)
    (print (str (clojure.string/join "\n" lines) "\n"))))

(defn gcd
  "Euclidean greatest common divisor of two positive integers."
  [a b]
  (loop [x a y b]
    (if (zero? y)
      x
      (recur y (rem x y)))))

(defn build-lg
  "Precompute floor(log2(i)) for i in 0..n."
  [n]
  (loop [i 2 acc [0 0]]
    (if (> i n)
      acc
      (recur (inc i) (conj acc (inc (nth acc (quot i 2))))))))

(defn build-table
  "Build sparse table levels for associative idempotent op f."
  [a n max-k f]
  (loop [j 1 half 1 len 2 levels [a]]
    (if (>= j max-k)
      levels
      (let [prev (nth levels (dec j))
            limit (- n len)
            row (loop [i 0 r []]
                  (if (> i limit)
                    r
                    (recur (inc i)
                           (conj r (f (nth prev i) (nth prev (+ i half)))))))]
        (recur (inc j) (* half 2) (* len 2) (conj levels row))))))

(defn parse-and-run
  "Parse tokens, construct sparse tables, and answer all Q queries."
  [toks]
  (if (< (count toks) 2)
    nil
    (let [n (parse-num (nth toks 0))
          q (parse-num (nth toks 1))]
      (if (or (nil? n) (nil? q) (<= n 0) (< q 0))
        nil
        (let [expected (+ 2 n (* 3 q))]
          (if (< (count toks) expected)
            nil
            (let [a (loop [i 0 ti 2 arr []]
                      (if (= i n)
                        arr
                        (let [v (parse-num (nth toks ti))]
                          (if (or (nil? v) (<= v 0))
                            nil
                            (recur (inc i) (inc ti) (conj arr v))))))]
              (if (nil? a)
                nil
                (let [lg (build-lg n)
                      max-k (inc (nth lg n))
                      pows (vec (take max-k (iterate (fn [x] (* x 2)) 1)))
                      st-min (build-table a n max-k min)
                      st-max (build-table a n max-k max)
                      st-gcd (build-table a n max-k gcd)
                      q-start (+ 2 n)]
                  (loop [qi 0 ti q-start out []]
                    (if (= qi q)
                      out
                      (let [qtype (parse-num (nth toks ti))
                            l (parse-num (nth toks (inc ti)))
                            r (parse-num (nth toks (+ ti 2)))]
                        (if (or (nil? qtype) (nil? l) (nil? r)
                                (< l 0) (< r l) (>= r n))
                          nil
                          (let [span (inc (- r l))
                                k (nth lg span)
                                r-start (- (inc r) (nth pows k))]
                            (case qtype
                              1 (let [row (nth st-min k)]
                                  (recur (inc qi) (+ ti 3)
                                         (conj out (str (min (nth row l) (nth row r-start))))))
                              2 (let [row (nth st-max k)]
                                  (recur (inc qi) (+ ti 3)
                                         (conj out (str (max (nth row l) (nth row r-start))))))
                              3 (let [row (nth st-gcd k)]
                                  (recur (inc qi) (+ ti 3)
                                         (conj out (str (gcd (nth row l) (nth row r-start))))))
                              nil)))))))))))))))

(defn -main
  "Entry point taking full stdin string."
  [input]
  (let [toks (tokenize input)
        lines (parse-and-run toks)]
    (if (nil? lines)
      (fail "invalid input for sparse_table")
      (emit-lines lines))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
