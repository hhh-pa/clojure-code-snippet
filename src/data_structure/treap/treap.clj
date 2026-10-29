(require 'clojure.string)

;; Optimal bounds: random priorities keep the expected height logarithmic, so
;; insert, find and erase cost expected O(log n) in O(n) space.

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

(defn rand-next
  "One step of a small deterministic generator."
  [s]
  (rem (+ (* s 1103515245) 12345) 2147483648))

(defn treap-split
  "Split t into [< k, >= k] by key, keeping heap order."
  [t k]
  (if (nil? t)
    [nil nil]
    (if (< k (:k t))
      (let [[ll lr] (treap-split (:l t) k)]
        [ll (assoc t :l lr)])
      (let [[rl rr] (treap-split (:r t) k)]
        [(assoc t :r rl) rr]))))

(defn treap-merge
  "Join l and r, where every key in l precedes every key in r."
  [l r]
  (cond
    (nil? l) r
    (nil? r) l
    (> (:p l) (:p r)) (assoc l :r (treap-merge (:r l) r))
    :else (assoc r :l (treap-merge l (:l r)))))

(defn treap-find
  "True when k sits in the treap."
  [t k]
  (loop [t t]
    (cond
      (nil? t) false
      (= k (:k t)) true
      (< k (:k t)) (recur (:l t))
      :else (recur (:r t)))))

(defn treap-insert
  "Insert k with priority p, assuming k is missing."
  [t k p]
  (cond
    (nil? t) {:k k :p p :l nil :r nil}
    (> p (:p t)) (let [[l r] (treap-split t k)]
                   {:k k :p p :l l :r r})
    (< k (:k t)) (assoc t :l (treap-insert (:l t) k p))
    :else (assoc t :r (treap-insert (:r t) k p))))

(defn treap-erase
  "Remove k when present, merging the orphaned children."
  [t k]
  (cond
    (nil? t) nil
    (= k (:k t)) (treap-merge (:l t) (:r t))
    (< k (:k t)) (assoc t :l (treap-erase (:l t) k))
    :else (assoc t :r (treap-erase (:r t) k))))

(defn inorder-keys
  "Keys of the treap in sorted order."
  [t]
  (when t
    (concat (inorder-keys (:l t)) [(:k t)] (inorder-keys (:r t)))))

(defn run-ops
  "Execute q treap ops from toks, returning lines or nil when invalid."
  [toks q]
  (let [nt (count toks)]
    (loop [i 0 ti 1 root nil rng 1337 out []]
      (if (or (>= i q) (>= ti nt))
        out
        (let [op (nth toks ti)]
          (cond
            (= op "INSERT")
            (if (>= (inc ti) nt)
              nil
              (let [v (parse-num (nth toks (inc ti)))]
                (if (nil? v)
                  nil
                  (if (treap-find root v)
                    (recur (inc i) (+ ti 2) root rng out)
                    (let [rng (rand-next rng)]
                      (recur (inc i) (+ ti 2)
                             (treap-insert root v rng) rng out))))))

            (= op "FIND")
            (if (>= (inc ti) nt)
              nil
              (let [v (parse-num (nth toks (inc ti)))]
                (if (nil? v)
                  nil
                  (recur (inc i) (+ ti 2) root rng
                         (conj out (if (treap-find root v) "FOUND" "NOT_FOUND"))))))

            (= op "ERASE")
            (if (>= (inc ti) nt)
              nil
              (let [v (parse-num (nth toks (inc ti)))]
                (if (nil? v)
                  nil
                  (recur (inc i) (+ ti 2) (treap-erase root v) rng out))))

            (= op "PRINT")
            (recur (inc i) (inc ti) root rng
                   (conj out (clojure.string/join " " (map str (inorder-keys root)))))

            :else
            (recur (inc i) (inc ti) root rng out)))))))

(defn -main
  "Run the treap script and print one line per query op."
  [input]
  (let [toks (tokenize input)
        q (parse-num (first toks))]
    (if (or (nil? q) (neg? q))
      (fail "invalid input: expected an op count q, then q treap ops")
      (let [lines (run-ops toks q)]
        (if (nil? lines)
          (fail "invalid input: treap ops need integer values")
          (emit-lines lines))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
