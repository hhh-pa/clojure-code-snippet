(require 'clojure.string)

;; Optimal bounds: sorting people by population and sweeping with two Fenwick
;; trees costs O(n log n) time and O(n) space per test case, which matches the
;; comparison lower bound for this pairwise wiring cost problem.

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

(def modulus
  "Prime used for every modular reduction below."
  1000000007)

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

(defn bit-add
  "Add val at 1-based position idx of the Fenwick tree in O(log n)."
  [bit n idx val]
  (loop [i idx]
    (when (<= i n)
      (assoc! bit i (rem (+' (nth bit i) val) modulus))
      (recur (+ i (bit-and i (- i)))))))

(defn bit-sum
  "Prefix sum through 1-based position idx in O(log n)."
  [bit idx]
  (loop [i idx acc 0]
    (if (pos? i)
      (recur (- i (bit-and i (- i))) (rem (+' acc (nth bit i)) modulus))
      acc)))

(defn binary-search
  "Index of target in sorted vector v, or -1 when it is missing."
  [v target]
  (loop [lo 0 hi (dec (count v))]
    (if (> lo hi)
      -1
      (let [mid (+ lo (quot (- hi lo) 2))
            got (nth v mid)]
        (cond
          (= got target) mid
          (< got target) (recur (inc mid) hi)
          :else (recur lo (dec mid)))))))

(defn case-answer
  "Wiring cost for one test case with positions xs and populations ps."
  [xs ps]
  (let [n (count xs)
        order (sort-by ps (range n))
        sorted-xs (vec (sort xs))
        counts (transient (vec (repeat (inc n) 0)))
        sums (transient (vec (repeat (inc n) 0)))]
    (loop [order order ans 0]
      (if (empty? order)
        ans
        (let [idx (first order)
              xi (nth xs idx)
              pi (rem (nth ps idx) modulus)
              rank (inc (binary-search sorted-xs xi))
              cnt-l (bit-sum counts rank)
              sum-l (bit-sum sums rank)
              tot-cnt (bit-sum counts n)
              tot-sum (bit-sum sums n)
              cnt-r (rem (+' (- tot-cnt cnt-l) modulus) modulus)
              sum-r (rem (+' (- tot-sum sum-l) modulus) modulus)
              xm (rem xi modulus)
              left-part (rem (+ (- (*' cnt-l xm) sum-l) modulus) modulus)
              right-part (rem (+ (- sum-r (*' cnt-r xm)) modulus) modulus)
              step-ans (rem (*' pi (rem (+' left-part right-part) modulus)) modulus)]
          (bit-add counts n rank 1)
          (bit-add sums n rank xi)
          (recur (rest order) (rem (+ ans step-ans) modulus)))))))

(defn parse-cases
  "Parse t test cases from toks, or nil when the shape is wrong."
  [toks]
  (let [t (parse-num (first toks))]
    (when (and t (not (neg? t)))
      (loop [tc 0 p 1 out []]
        (if (>= tc t)
          out
          (let [n (when (< p (count toks)) (parse-num (nth toks p)))
                have (- (count toks) (inc p))]
            (if (or (nil? n) (neg? n) (< have (*' 2 n)))
              nil
              (let [xs (mapv parse-num (take n (drop (inc p) toks)))
                    ps (mapv parse-num (take n (drop (+ p 1 n) toks)))]
                (if (or (some nil? xs) (some nil? ps))
                  nil
                  (recur (inc tc) (+ p 1 (* 2 n)) (conj out [xs ps])))))))))))

(defn -main
  "Solve every test case and print one answer line each."
  [input]
  (let [cases (parse-cases (tokenize input))]
    (if (nil? cases)
      (fail "invalid input: expected t test cases of n, then n xs, then n pops")
      (emit-lines (mapv (fn [[xs ps]] (str (case-answer xs ps))) cases)))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
