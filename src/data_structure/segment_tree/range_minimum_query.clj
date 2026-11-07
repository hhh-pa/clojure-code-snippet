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
  (when (and (string? tok) (re-matches #"-?[0-9]+" tok))
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
    (print (str (clojure.string/join "\n" lines) "\n"))))

(defn build-tree!
  "Fill transient tree nodes for interval [l r] from a."
  [tree a node l r]
  (if (= l r)
    (assoc! tree node (nth a l))
    (let [m (quot (+ l r) 2)
          left (* 2 node)
          right (inc left)
          tree (build-tree! tree a left l m)
          tree (build-tree! tree a right (inc m) r)
          lv (nth tree left)
          rv (nth tree right)]
      (assoc! tree node (if (<= lv rv) lv rv)))))

(defn range-min
  "Minimum over [ql qr] within the node interval [l r]."
  [tree node l r ql qr]
  (cond
    (or (< r ql) (< qr l)) no-overlap
    (and (<= ql l) (<= r qr)) (nth tree node)
    :else (let [m (quot (+ l r) 2)
                lv (range-min tree (* 2 node) l m ql qr)
                rv (range-min tree (inc (* 2 node)) (inc m) r ql qr)]
            (if (<= lv rv) lv rv))))

(defn point-update!
  "Set position idx to val in transient tree."
  [tree node l r idx val]
  (if (= l r)
    (assoc! tree node val)
    (let [m (quot (+ l r) 2)
          left (* 2 node)
          right (inc left)
          tree (if (<= idx m)
                 (point-update! tree left l m idx val)
                 (point-update! tree right (inc m) r idx val))
          lv (nth tree left)
          rv (nth tree right)]
      (assoc! tree node (if (<= lv rv) lv rv)))))

(defn run-ops
  "Apply q ops from toks at ti over transient tree.
   Returns [lines truncated?] with lines nil when an op is malformed."
  [toks q ti tree n]
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
                     (conj! out (str (range-min tree 1 0 (dec n) (dec x) (dec y)))))
              (recur (inc i) (+ ti 3)
                     (point-update! tree 1 0 (dec n) (dec x) y) out))))))))

(defn -main
  "Build the minimum tree, then answer queries and point updates."
  [input]
  (let [toks (tokenize input)
        n (parse-num (first toks))
        q (parse-num (second toks))]
    (if (or (nil? n) (nil? q) (neg? n) (neg? q) (< (count toks) (+' 2 n)))
      (fail "invalid input: expected n and q, then n values, then q ops")
      (let [a (loop [i 0 acc (transient [])]
                (if (= i n)
                  (persistent! acc)
                  (let [v (parse-num (nth toks (+ 2 i)))]
                    (if (nil? v)
                      nil
                      (recur (inc i) (conj! acc v))))))]
        (if (nil? a)
          (fail "invalid input: expected n and q, then n values, then q ops")
          (let [tree (if (pos? n)
                       (build-tree! (transient (vec (repeat (* 4 n) 0))) a 1 0 (dec n))
                       (transient (vec (repeat 4 0))))
                [lines truncated?] (run-ops toks q (+ 2 n) tree n)]
            (cond
              (nil? lines)
              (fail "invalid input: ops need two integer operands each")
              truncated?
              (do
                (emit-lines lines)
                (fail "invalid input: the final op is cut short"))
              :else (emit-lines lines))))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
