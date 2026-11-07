(require 'clojure.string)

;; Optimal bounds: union by size with path halving gives amortized
;; near-constant time per op and O(n) space, the best known practical bound.

(def max-long
  "Largest signed 64-bit value; tokens beyond long range are invalid."
  9223372036854775807)

(def min-long
  "Smallest signed 64-bit value."
  -9223372036854775808)

(defn tokenize
  "Split s into whitespace separated tokens in O(n) time."
  [s]
  (if (nil? s)
    []
    (let [v (clojure.string/split s #"[ \t\n\r]+")
          n (count v)
          start (if (and (pos? n) (clojure.string/blank? (nth v 0))) 1 0)
          end (if (and (> n start) (clojure.string/blank? (nth v (dec n)))) (dec n) n)]
      (subvec v start end))))

(defn parse-num
  "Parse tok as an integer, or nil when it is not one."
  [tok]
  (when (and (string? tok) (pos? (count tok)) (not= (nth tok 0) \+))
    (when-let [v (parse-long tok)]
      (if (= v -9223372036854775808) (bigint v) v))))

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
    (println (clojure.string/join "\n" lines))))

(defn find-root-vec!
  "Root of x in transient int vector parent, halving paths along the way."
  [parent x]
  (loop [r x]
    (let [p (nth parent r)]
      (if (= p r)
        r
        (let [gp (nth parent p)]
          (assoc! parent r gp)
          (recur gp))))))

(defn find-root-map
  "Root of x in transient map parent, halving paths along the way."
  [parent x]
  (loop [r x parent parent]
    (let [p (get parent r r)]
      (if (= p r)
        [r parent]
        (recur p (assoc! parent r (get parent p p)))))))

(defn process-pairs
  "Union each token pair, collecting one output line per pair."
  [toks n]
  (let [nt (count toks)]
    (if (<= n 262144)
      (let [parent (transient (vec (range n)))
            size (transient (vec (repeat n 1)))]
        (loop [i 1 comps n lines (transient [])]
          (if (>= (+ i 1) nt)
            (persistent! lines)
            (let [u (parse-num (nth toks i))
                  v (parse-num (nth toks (inc i)))]
              (if (or (nil? u) (nil? v) (< u 1) (> u n) (< v 1) (> v n))
                nil
                (let [ru (find-root-vec! parent (dec u))
                      rv (find-root-vec! parent (dec v))]
                  (if (= ru rv)
                    (recur (+ i 2) comps (conj! lines (str comps " CYCLE FORMED!")))
                    (let [su (nth size ru)
                          sv (nth size rv)]
                      (if (>= su sv)
                        (do (assoc! parent rv ru)
                            (assoc! size ru (+ su sv)))
                        (do (assoc! parent ru rv)
                            (assoc! size rv (+ su sv))))
                      (recur (+ i 2) (dec comps) (conj! lines (str (dec comps))))))))))))
      (loop [i 1 comps n lines (transient []) parent (transient {}) size (transient {})]
        (if (>= (+ i 1) nt)
          (persistent! lines)
          (let [u (parse-num (nth toks i))
                v (parse-num (nth toks (inc i)))]
            (if (or (nil? u) (nil? v) (< u 1) (> u n) (< v 1) (> v n))
              nil
              (let [[ru parent] (find-root-map parent (dec u))
                    [rv parent] (find-root-map parent (dec v))]
                (if (= ru rv)
                  (recur (+ i 2) comps (conj! lines (str comps " CYCLE FORMED!"))
                         parent size)
                  (let [su (get size ru 1)
                        sv (get size rv 1)
                        [big small] (if (>= su sv) [ru rv] [rv ru])]
                    (recur (+ i 2) (dec comps) (conj! lines (str (dec comps)))
                           (assoc! parent small big)
                           (assoc! size big (+ su sv)))))))))))))

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
