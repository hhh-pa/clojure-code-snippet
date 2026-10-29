(require 'clojure.string)

;; Optimal bounds: a sorted map of value to multiplicity gives O(log n) insert
;; and remove plus O(n) ordered traversal in O(n) space. The old sorted buffer
;; shifted O(n) cells per update, so this is asymptotically faster.

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

(defn multiset-insert
  "Add one copy of v to the sorted multiset."
  [tree v]
  (assoc tree v (inc (get tree v 0))))

(defn multiset-remove
  "Remove one copy of v, which must be present."
  [tree v]
  (let [c (get tree v 0)]
    (if (= c 1)
      (dissoc tree v)
      (assoc tree v (dec c)))))

(defn multiset-line
  "Render the whole multiset in sorted order on one line."
  [tree]
  (clojure.string/join " " (mapcat (fn [[v c]] (repeat c (str v))) tree)))

(defn run-ops
  "Apply the op tokens from index 1, threading tree, total and lines."
  [toks]
  (loop [i 1 tree (sorted-map) total 0 lines []]
    (if (>= i (count toks))
      lines
      (let [op (nth toks i)]
        (cond
          (= op "i")
          (let [has-val (< (inc i) (count toks))]
            (if-not has-val
              (recur (+ i 2) tree total lines)
              (let [v (parse-num (nth toks (inc i)))]
                (if (nil? v)
                  nil
                  (recur (+ i 2) (multiset-insert tree v) (inc total) lines)))))

          (= op "r")
          (if-not (< (inc i) (count toks))
            (recur (+ i 2) tree total lines)
            (let [v (parse-num (nth toks (inc i)))]
              (if (nil? v)
                nil
                (cond
                  (zero? total)
                  (recur (+ i 2) tree total (conj lines "The tree is empty"))
                  (zero? (get tree v 0))
                  (recur (+ i 2) tree total
                         (conj lines (str "The key " v " is does not exist in the tree")))
                  :else
                  (recur (+ i 2) (multiset-remove tree v) (dec total) lines)))))

          (= op "t")
          (recur (inc i) tree total (conj lines (multiset-line tree)))

          :else
          (recur (inc i) tree total lines))))))

(defn -main
  "Run insert, remove and traverse ops over the sorted multiset."
  [input]
  (let [lines (run-ops (tokenize input))]
    (if (nil? lines)
      (fail "invalid input: expected integer values after insert and remove ops")
      (emit-lines lines))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
