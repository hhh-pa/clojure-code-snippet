(require 'clojure.string)

;; Optimal bounds: union by size with path halving gives amortized
;; near-constant time per op and O(n) space, the best known practical bound.

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

(defn find-root
  "Root of x in the parent map, halving paths along the way.
   Unseen nodes are their own roots. Returns [root parent'] since
   assoc! on a transient map may promote (and replace) the object."
  [parent x]
  (loop [r x parent parent]
    (let [p (get parent r r)]
      (if (= p r)
        [r parent]
        (recur p (assoc! parent r (get parent p p)))))))

(defn process-pairs
  "Union each token pair, collecting one output line per pair.
   Sparse maps keep time/space proportional to the pairs seen, so a
   huge node count with few pairs stays cheap; isolated nodes never
   materialize yet still count through comps."
  [toks n]
  (loop [i 1 comps n lines [] parent (transient {}) size (transient {})]
    (if (>= (+ i 1) (count toks))
      lines
      (let [u (parse-num (nth toks i))
            v (parse-num (nth toks (inc i)))]
        (if (or (nil? u) (nil? v) (< u 1) (> u n) (< v 1) (> v n))
          nil
          (let [[ru parent] (find-root parent (dec u))
                [rv parent] (find-root parent (dec v))]
            (if (= ru rv)
              (recur (+ i 2) comps (conj lines (str comps " CYCLE FORMED!"))
                     parent size)
              (let [su (get size ru 1)
                    sv (get size rv 1)
                    [big small] (if (>= su sv) [ru rv] [rv ru])]
                (recur (+ i 2) (dec comps) (conj lines (str (dec comps)))
                       (assoc! parent small big)
                       (assoc! size big (+ su sv)))))))))))

(defn -main
  "Read n plus edge pairs, printing the live component count per edge."
  [input]
  (let [toks (tokenize input)
        n (parse-num (first toks))]
    (if (or (nil? n) (neg? n))
      (fail "invalid input: expected a node count n, then edge pairs")
      (let [lines (process-pairs toks n)]
        (if (nil? lines)
          (fail "invalid input: edge endpoints must sit within 1 and n")
          (emit-lines lines))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
