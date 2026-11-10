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

(defn chunk-tokens
  "Tokenize one block of lines: pre-parsed ints, or token strings
   when the block contains a '+'."
  [lines]
  (let [text (clojure.string/join "\n" lines)
        toks (tokenize text)]
    (if (clojure.string/includes? text "+")
      toks
      (mapv parse-long toks))))

(defn read-chunk
  "The next block of input tokens (about 1MB of whole lines), or nil
   at EOF. Blocks without a '+' come back pre-parsed by the bulk
   mapv parse-long (nil for a bad token); a block containing '+'
   stays as token strings for the per-token path (parse-num rejects
   a leading '+', parse-long accepts it). Reading block-wise keeps
   only one block of input alive while the union loop runs."
  []
  (loop [lines (transient []) chars 0]
    (if (>= chars 1000000)
      (chunk-tokens (persistent! lines))
      (if-let [line (read-line)]
        (recur (conj! lines line) (+ chars (count line)))
        (let [v (persistent! lines)]
          (when (seq v)
            (chunk-tokens v)))))))
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

(defn render-chunk
  "Render one chunk of encoded lines to its joined string. Lines are
   ints: a component count, or its negation for a cycle line (counts
   stay >= 1, so the sign is unambiguous)."
  [lines]
  (let [n (count lines)
        parts (loop [i 0 ps (transient [])]
                (if (= i n)
                  (persistent! ps)
                  (let [x (nth lines i)]
                    (recur (inc i)
                           (conj! ps (if (neg? x)
                                       (str (- x) " CYCLE FORMED!")
                                       (str x)))))))]
    (clojure.string/join "\n" parts)))

(defn emit-chunks
  "Print the rendered output chunks, or nothing when there are none."
  [chunks]
  (when (seq chunks)
    (println (clojure.string/join "\n" chunks))))

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

(defn chunk-dense
  "Union pairs from toks starting at index i (a pending first token
   may lead). Returns [comps cyc lines pending] or nil on an invalid
   endpoint. Finds run inline with the same path halving as
   find-root-vec!."
  [ds n toks i pending comps cyc lines]
  (let [nt (count toks)]
    (loop [i i pending pending comps comps cyc cyc lines lines]
      (if (>= i nt)
        [comps cyc lines pending]
        (if (and (nil? pending) (>= (inc i) nt))
          [comps cyc lines (nth toks i)]
          (let [tu (or pending (nth toks i))
                i (if (some? pending) i (inc i))
                u (if (string? tu) (parse-num tu) tu)
                tv (nth toks i)
                v (if (string? tv) (parse-num tv) tv)]
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
                  (let [cs (or cyc (- comps))]
                    (recur (inc i) nil comps cs (conj! lines cs)))
                  (let [su (nth ds ru)
                        sv (nth ds rv)
                        ncomps (dec comps)]
                    (if (<= su sv)
                      (do (assoc! ds rv ru)
                          (assoc! ds ru (+ su sv)))
                      (do (assoc! ds ru rv)
                          (assoc! ds rv (+ su sv))))
                    (recur (inc i) nil ncomps nil (conj! lines ncomps))))))))))))

(defn chunk-sparse
  "Union pairs from toks starting at index i (sparse map sets).
   Returns [comps lines parent size pending] or nil on an invalid
   endpoint."
  [n toks i pending comps lines parent size]
  (let [nt (count toks)]
    (loop [i i pending pending comps comps lines lines parent parent size size]
      (if (>= i nt)
        [comps lines parent size pending]
        (if (and (nil? pending) (>= (inc i) nt))
          [comps lines parent size (nth toks i)]
          (let [tu (or pending (nth toks i))
                i (if (some? pending) i (inc i))
                u (if (string? tu) (parse-num tu) tu)
                tv (nth toks i)
                v (if (string? tv) (parse-num tv) tv)]
            (if (or (nil? u) (nil? v) (< u 1) (> u n) (< v 1) (> v n))
              nil
              (let [[ru parent] (find-root-map parent (dec u))
                    [rv parent] (find-root-map parent (dec v))]
                (if (= ru rv)
                  (recur (inc i) nil comps (conj! lines (- comps)) parent size)
                  (let [su (get size ru 1)
                        sv (get size rv 1)
                        [big small] (if (>= su sv) [ru rv] [rv ru])]
                    (recur (inc i) nil (dec comps) (conj! lines (dec comps))
                           (assoc! parent small big)
                           (assoc! size big (+ su sv)))))))))))))

(defn run-dense
  "Drive the dense union loop over chunked input; rendered output
   chunks, or nil on an invalid endpoint."
  [n first-chunk]
  (let [ds (transient (vec (repeat n -1)))]
    (loop [chunk first-chunk start 1 comps n cyc nil pending nil out (transient [])]
      (if (nil? chunk)
        (persistent! out)
        (let [lines (transient [])
              state (chunk-dense ds n chunk start pending comps cyc lines)]
          (when state
            (let [[comps cyc lines pending] state
                  out (if (pos? (count lines))
                        (conj! out (render-chunk (persistent! lines)))
                        out)]
              (recur (read-chunk) 0 comps cyc pending out))))))))

(defn run-sparse
  "Drive the sparse union loop over chunked input; rendered output
   chunks, or nil on an invalid endpoint."
  [n first-chunk]
  (loop [chunk first-chunk start 1 comps n pending nil
         parent (transient {}) size (transient {}) out (transient [])]
    (if (nil? chunk)
      (persistent! out)
      (let [lines (transient [])
            state (chunk-sparse n chunk start pending comps lines parent size)]
        (when state
          (let [[comps lines parent size pending] state
                out (if (pos? (count lines))
                      (conj! out (render-chunk (persistent! lines)))
                      out)]
            (recur (read-chunk) 0 comps pending parent size out)))))))

(defn -main
  "Read n plus edge pairs, printing the live component count per edge."
  []
  (when-let [first-chunk (read-chunk)]
    (let [t0 (nth first-chunk 0)
          n (if (string? t0) (parse-num t0) t0)]
      (if (or (nil? n) (neg? n))
        (fail "invalid input: expected a node count n, then edge pairs")
        (let [chunks (if (<= n 16777216)
                       (run-dense n first-chunk)
                       (run-sparse n first-chunk))]
          (if (nil? chunks)
            (fail "invalid input: edge endpoints must sit within 1 and n")
            (emit-chunks chunks)))))))

(-main)
