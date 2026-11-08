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
    (parse-long tok)))

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
  "Root of x in single-array transient disjoint set ds (negative = -size, >= 0 = parent)."
  [ds x]
  (let [p (nth ds x)]
    (if (neg? p)
      x
      (loop [r x p p]
        (let [gp (nth ds p)]
          (if (neg? gp)
            p
            (do (assoc! ds r gp)
                (let [ggp (nth ds gp)]
                  (if (neg? ggp)
                    gp
                    (recur gp ggp))))))))))

(defn find-root-map
  "Root of x in transient map parent, halving paths along the way."
  [parent x]
  (loop [r x parent parent]
    (let [p (get parent r r)]
      (if (= p r)
        [r parent]
        (recur p (assoc! parent r (get parent p p)))))))

(defn process-ints-dense
  "Union each pair of the pre-parsed ints (nil marks a bad token), dense path.
   Finds run inline with the same path halving as find-root-vec!."
  [ints n]
  (let [nt (count ints)
        ds (transient (vec (repeat n -1)))]
    (loop [i 1 comps n cyc-str nil lines (transient [])]
      (if (>= (+ i 1) nt)
        (persistent! lines)
        (let [u (nth ints i)
              v (nth ints (inc i))]
          (if (or (nil? u) (nil? v) (< u 1) (> u n) (< v 1) (> v n))
            nil
            (let [x (dec u)
                  px (nth ds x)
                  ru (if (neg? px)
                       x
                       (loop [r x p px]
                         (let [gp (nth ds p)]
                           (if (neg? gp)
                             p
                             (do (assoc! ds r gp)
                                 (let [ggp (nth ds gp)]
                                   (if (neg? ggp)
                                     gp
                                     (recur gp ggp))))))))
                  y (dec v)
                  py (nth ds y)
                  rv (if (neg? py)
                       y
                       (loop [r y p py]
                         (let [gp (nth ds p)]
                           (if (neg? gp)
                             p
                             (do (assoc! ds r gp)
                                 (let [ggp (nth ds gp)]
                                   (if (neg? ggp)
                                     gp
                                     (recur gp ggp))))))))]
              (if (= ru rv)
                (let [cs (or cyc-str (str comps " CYCLE FORMED!"))]
                  (recur (+ i 2) comps cs (conj! lines cs)))
                (let [su (nth ds ru)
                      sv (nth ds rv)
                      ncomps (dec comps)]
                  (if (<= su sv)
                    (do (assoc! ds rv ru)
                        (assoc! ds ru (+ su sv)))
                    (do (assoc! ds ru rv)
                        (assoc! ds rv (+ su sv))))
                  (recur (+ i 2) ncomps nil (conj! lines (str ncomps))))))))))))

(defn process-ints-sparse
  "Union each pair of the pre-parsed ints (nil marks a bad token), sparse path."
  [ints n]
  (let [nt (count ints)]
    (loop [i 1 comps n lines (transient []) parent (transient {}) size (transient {})]
      (if (>= (+ i 1) nt)
        (persistent! lines)
        (let [u (nth ints i)
              v (nth ints (inc i))]
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
                         (assoc! size big (+ su sv))))))))))))

(defn process-pairs
  "Union each token pair, collecting one output line per pair."
  [toks n]
  (let [nt (count toks)]
    (if (<= n 16777216)
      (let [ds (transient (vec (repeat n -1)))]
        (loop [i 1 comps n cyc-str nil lines (transient [])]
          (if (>= (+ i 1) nt)
            (persistent! lines)
            (let [u (parse-num (nth toks i))
                  v (parse-num (nth toks (inc i)))]
              (if (or (nil? u) (nil? v) (< u 1) (> u n) (< v 1) (> v n))
                nil
                (let [ru (find-root-vec! ds (dec u))
                      rv (find-root-vec! ds (dec v))]
                  (if (= ru rv)
                    (let [cs (or cyc-str (str comps " CYCLE FORMED!"))]
                      (recur (+ i 2) comps cs (conj! lines cs)))
                    (let [su (nth ds ru)
                          sv (nth ds rv)
                          ncomps (dec comps)]
                      (if (<= su sv)
                        (do (assoc! ds rv ru)
                            (assoc! ds ru (+ su sv)))
                        (do (assoc! ds ru rv)
                            (assoc! ds rv (+ su sv))))
                      (recur (+ i 2) ncomps nil (conj! lines (str ncomps)))))))))))
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

(defn prepare
  "Tokenize input and decide the payload: [n ints] with every token parsed by
   parse-long (nil for a bad token), [n toks] when a '+' appears anywhere
   (parse-num rejects a leading '+', parse-long accepts it, so such inputs
   keep the per-token path), or nil for blank input. Returning from this
   frame lets the token strings die before the union loop runs."
  [input]
  (when-not (clojure.string/blank? input)
    (let [toks (tokenize input)]
      (when (seq toks)
        (let [n (parse-num (first toks))]
          (if (clojure.string/includes? input "+")
            [n toks]
            [n (mapv parse-long (vec toks))]))))))

(defn -main
  "Read n plus edge pairs, printing the live component count per edge."
  []
  (let [prep (prepare (slurp *in*))]
    (when prep
      (let [[n payload] prep]
        (if (or (nil? n) (neg? n))
          (fail "invalid input: expected a node count n, then edge pairs")
          (let [lines (cond
                        (string? (first payload)) (process-pairs payload n)
                        (<= n 16777216) (process-ints-dense payload n)
                        :else (process-ints-sparse payload n))]
            (if (nil? lines)
              (fail "invalid input: edge endpoints must sit within 1 and n")
              (emit-lines lines))))))))

(-main)
