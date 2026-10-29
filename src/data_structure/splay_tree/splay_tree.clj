(require 'clojure.string)

;; Optimal bounds: bottom-up splaying gives amortized O(log n) insert, find
;; and delete in O(n) space, the classic self-adjusting guarantee.

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

(defn rot-left
  "Single left rotation, returning the new subtree root."
  [t]
  (let [r (:r t)]
    (assoc r :l (assoc t :r (:l r)))))

(defn rot-right
  "Single right rotation, returning the new subtree root."
  [t]
  (let [l (:l t)]
    (assoc l :r (assoc t :l (:r l)))))

(defn splay
  "Splay k to the root, or the last node touched when k is missing."
  [t k]
  (cond
    (or (nil? t) (= k (:k t))) t
    (< k (:k t))
    (let [l (:l t)]
      (if (nil? l)
        t
        (cond
          (= k (:k l)) (rot-right t)
          (< k (:k l))
          (if (nil? (:l l))
            (rot-right t)
            (let [ll (splay (:l l) k)
                  l (assoc l :l ll)
                  t (assoc t :l (rot-right l))]
              (rot-right t)))
          (nil? (:r l)) (rot-right t)
          :else
          (let [lr (splay (:r l) k)
                l (assoc l :r lr)
                t (assoc t :l (rot-left l))]
            (rot-right t)))))
    :else
    (let [r (:r t)]
      (if (nil? r)
        t
        (cond
          (= k (:k r)) (rot-left t)
          (> k (:k r))
          (if (nil? (:r r))
            (rot-left t)
            (let [rr (splay (:r r) k)
                  r (assoc r :r rr)
                  t (assoc t :r (rot-left r))]
              (rot-left t)))
          (nil? (:l r)) (rot-left t)
          :else
          (let [rl (splay (:l r) k)
                r (assoc r :l rl)
                t (assoc t :r (rot-right r))]
            (rot-left t)))))))

(defn splay-insert
  "Insert k unless present, splaying the result to the root."
  [t k]
  (if (nil? t)
    {:k k :l nil :r nil}
    (let [t (splay t k)]
      (cond
        (= k (:k t)) t
        (< k (:k t)) {:k k :l (:l t) :r (assoc t :l nil)}
        :else {:k k :r (:r t) :l (assoc t :r nil)}))))

(defn splay-max
  "Splay the maximum key of t to the root."
  [t]
  (if (or (nil? t) (nil? (:r t)))
    t
    (let [r (:r t)]
      (if (nil? (:r r))
        (rot-left t)
        (let [rr (splay-max (:r r))]
          (rot-left (assoc t :r (rot-left (assoc r :r rr)))))))))

(defn splay-delete
  "Remove k when present, joining the remaining halves."
  [t k]
  (if (nil? t)
    nil
    (let [t (splay t k)]
      (if (not= k (:k t))
        t
        (let [l (:l t) r (:r t)]
          (if (nil? l)
            r
            (assoc (splay-max l) :r r)))))))

(defn inorder-keys
  "Keys of the tree in sorted order."
  [t]
  (when t
    (concat (inorder-keys (:l t)) [(:k t)] (inorder-keys (:r t)))))

(defn run-ops
  "Execute q tree ops from toks, returning lines or nil when invalid."
  [toks q]
  (let [nt (count toks)]
    (loop [i 0 ti 1 root nil out []]
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
                  (recur (inc i) (+ ti 2) (splay-insert root v) out))))

            (= op "FIND")
            (if (>= (inc ti) nt)
              nil
              (let [v (parse-num (nth toks (inc ti)))]
                (if (nil? v)
                  nil
                  (let [root (splay root v)]
                    (recur (inc i) (+ ti 2) root
                           (conj out (if (and root (= (:k root) v))
                                       "FOUND"
                                       "NOT_FOUND")))))))

            (= op "DELETE")
            (if (>= (inc ti) nt)
              nil
              (let [v (parse-num (nth toks (inc ti)))]
                (if (nil? v)
                  nil
                  (recur (inc i) (+ ti 2) (splay-delete root v) out))))

            (= op "PRINT")
            (recur (inc i) (inc ti) root
                   (conj out (clojure.string/join " " (map str (inorder-keys root)))))

            :else
            (recur (inc i) (inc ti) root out)))))))

(defn -main
  "Run the splay script and print one line per query op."
  [input]
  (let [toks (tokenize input)
        q (parse-num (first toks))]
    (if (or (nil? q) (neg? q))
      (fail "invalid input: expected an op count q, then q tree ops")
      (let [lines (run-ops toks q)]
        (if (nil? lines)
          (fail "invalid input: tree ops need integer values")
          (emit-lines lines))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
