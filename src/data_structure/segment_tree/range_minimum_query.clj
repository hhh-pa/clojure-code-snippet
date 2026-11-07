(require 'clojure.string)

;; Optimal bounds: the tree is built in O(n), then each range minimum query
;; and point update costs O(log n) in O(n) space, the classic segment bound.

(def max-long
  "Largest signed 64-bit value; tokens beyond long range are invalid."
  9223372036854775807)

(def min-long
  "Smallest signed 64-bit value."
  -9223372036854775808)

(def no-overlap
  "Sentinel for disjoint query ranges, matching integer max value."
  2147483647)

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

(defn range-min
  "Minimum over [ql qr] (0-indexed inclusive) in iterative segment tree of leaf base size."
  [tree size ql qr]
  (loop [l (+ ql size) r (+ qr size 1) res no-overlap]
    (if (>= l r)
      res
      (let [res1 (if (odd? l)
                   (let [v (nth tree l)] (if (< v res) v res))
                   res)
            l1 (if (odd? l) (inc l) l)
            r1 (if (odd? r) (dec r) r)
            res2 (if (odd? r)
                   (let [v (nth tree r1)] (if (< v res1) v res1))
                   res1)]
        (recur (bit-shift-right l1 1) (bit-shift-right r1 1) res2)))))

(defn point-update!
  "Set position idx (0-indexed) to val in iterative transient segment tree."
  [tree size idx val]
  (let [pos (+ idx size)]
    (assoc! tree pos val)
    (loop [p (bit-shift-right pos 1)]
      (if (<= p 0)
        tree
        (let [lv (nth tree (bit-shift-left p 1))
              rv (nth tree (inc (bit-shift-left p 1)))]
          (assoc! tree p (if (<= lv rv) lv rv))
          (recur (bit-shift-right p 1)))))))

(defn run-ops
  "Apply q ops from toks at ti over transient tree.
   Returns [lines truncated?] with lines nil when an op is malformed."
  [toks q ti tree size]
  (let [nt (count toks)]
    (loop [i 0 ti ti tree tree out (transient [])]
      (cond
        (or (>= i q) (>= ti nt)) [(persistent! out) false]
        (>= (+ ti 2) nt) [(persistent! out) true]
        :else
        (let [typ (nth toks ti)
              x (parse-num (nth toks (+ ti 1)))
              y (parse-num (nth toks (+ ti 2)))]
          (if (or (nil? x) (nil? y))
            [nil false]
            (if (= typ "q")
              (recur (inc i) (+ ti 3) tree
                     (conj! out (str (range-min tree size (dec x) (dec y)))))
              (recur (inc i) (+ ti 3)
                     (point-update! tree size (dec x) y) out))))))))

(defn -main
  "Build the minimum tree, then answer queries and point updates."
  [toks]
  (let [n (parse-num (first toks))
        q (parse-num (second toks))]
    (if (or (nil? n) (nil? q) (neg? n) (neg? q) (< (count toks) (+' 2 n)))
      (fail "invalid input: expected n and q, then n values, then q ops")
      (let [size (loop [s 1] (if (>= s n) s (recur (bit-shift-left s 1))))
            tree (transient (vec (repeat (* 2 size) no-overlap)))
            ok? (loop [i 0]
                  (if (= i n)
                    true
                    (let [v (parse-num (nth toks (+ 2 i)))]
                      (if (nil? v)
                        false
                        (do (assoc! tree (+ size i) v)
                            (recur (inc i)))))))]
        (if-not ok?
          (fail "invalid input: expected n and q, then n values, then q ops")
          (do
            (loop [i (dec size)]
              (when (pos? i)
                (let [lv (nth tree (bit-shift-left i 1))
                      rv (nth tree (inc (bit-shift-left i 1)))]
                  (assoc! tree i (if (<= lv rv) lv rv))
                  (recur (dec i)))))
            (let [[lines truncated?] (run-ops toks q (+ 2 n) tree size)]
              (cond
                (nil? lines)
                (fail "invalid input: ops need two integer operands each")
                truncated?
                (do
                  (emit-lines lines)
                  (fail "invalid input: the final op is cut short"))
                :else (emit-lines lines)))))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks)
    (-main toks)))
