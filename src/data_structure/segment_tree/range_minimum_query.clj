(require 'clojure.string)

;; Optimal bounds: the tree is built in O(n), then each range minimum query
;; and point update costs O(log n) in O(n) space, the classic segment bound.

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

(def no-overlap
  "Sentinel for disjoint query ranges, matching integer max value."
  2147483647)

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

(defn build-tree
  "Fill tree nodes for interval [l r] from a, returning the new tree."
  [tree a node l r]
  (if (= l r)
    (assoc tree node (nth a l))
    (let [m (quot (+ l r) 2)
          left (* 2 node)
          right (inc left)
          tree (build-tree tree a left l m)
          tree (build-tree tree a right (inc m) r)]
      (assoc tree node (min (nth tree left) (nth tree right))))))

(defn range-min
  "Minimum over [ql qr] within the node interval [l r]."
  [tree node l r ql qr]
  (cond
    (or (< r ql) (< qr l)) no-overlap
    (and (<= ql l) (<= r qr)) (nth tree node)
    :else (let [m (quot (+ l r) 2)]
            (min (range-min tree (* 2 node) l m ql qr)
                 (range-min tree (inc (* 2 node)) (inc m) r ql qr)))))

(defn point-update
  "Set position idx to val, returning the new tree."
  [tree node l r idx val]
  (if (= l r)
    (assoc tree node val)
    (let [m (quot (+ l r) 2)
          left (* 2 node)
          right (inc left)
          tree (if (<= idx m)
                 (point-update tree left l m idx val)
                 (point-update tree right (inc m) r idx val))]
      (assoc tree node (min (nth tree left) (nth tree right))))))

(defn run-ops
  "Apply q ops from toks at ti over tree.
   Returns [lines truncated?] with lines nil when an op is malformed."
  [toks q ti tree n]
  (let [nt (count toks)]
    (loop [i 0 ti ti tree tree out []]
      (cond
        (or (>= i q) (>= ti nt)) [out false]
        (>= (+ ti 2) nt) [out true]
        :else
        (let [typ (nth toks ti)
              x (parse-num (nth toks (+ ti 1)))
              y (parse-num (nth toks (+ ti 2)))]
          (if (or (nil? x) (nil? y))
            [nil false]
            (if (= typ "q")
              (recur (inc i) (+ ti 3) tree
                     (conj out (str (range-min tree 1 0 (dec n) (dec x) (dec y)))))
              (recur (inc i) (+ ti 3)
                     (point-update tree 1 0 (dec n) (dec x) y) out))))))))

(defn -main
  "Build the minimum tree, then answer queries and point updates."
  [input]
  (let [toks (tokenize input)
        n (parse-num (first toks))
        q (parse-num (second toks))]
    (if (or (nil? n) (nil? q) (neg? n) (neg? q) (< (count toks) (+' 2 n)))
      (fail "invalid input: expected n and q, then n values, then q ops")
      (let [a (mapv parse-num (take n (drop 2 toks)))]
        (if (some nil? a)
          (fail "invalid input: expected n and q, then n values, then q ops")
          (let [tree (if (pos? n)
                       (build-tree (vec (repeat (* 4 n) 0)) a 1 0 (dec n))
                       (vec (repeat 4 0)))
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
