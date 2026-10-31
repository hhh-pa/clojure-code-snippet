(require 'clojure.string)

;; Optimal bounds: Red-Black Tree maintains height O(log n), giving worst-case
;; O(log n) insert, delete, search, predecessor, and successor in O(n) space.

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
  "Report invalid input on stderr and return normally."
  [msg]
  (binding [*out* *err*]
    (println msg)))

(defn emit-lines
  "Print each line with a trailing newline, or nothing when there are none."
  [lines]
  (when (seq lines)
    (print (str (clojure.string/join "\n" lines) "\n"))))

(defn red?
  "True when node h is non-nil and colored red."
  [h]
  (and (some? h) (= (:c h) :red)))

(defn flip-color
  "Toggle :red and :black."
  [c]
  (if (= c :red) :black :red))

(defn rotate-left
  "Left-rotate red right link of h."
  [h]
  (let [x (:r h)
        h2 (assoc h :r (:l x) :c :red)]
    (assoc x :l h2 :c (:c h))))

(defn rotate-right
  "Right-rotate red left link of h."
  [h]
  (let [x (:l h)
        h2 (assoc h :l (:r x) :c :red)]
    (assoc x :r h2 :c (:c h))))

(defn flip-colors
  "Flip colors of h and its two children."
  [h]
  (assoc h
         :c (flip-color (:c h))
         :l (assoc (:l h) :c (flip-color (:c (:l h))))
         :r (assoc (:r h) :c (flip-color (:c (:r h))))))

(defn balance
  "Restore red-black invariants on node h."
  [h]
  (let [h (if (and (red? (:r h)) (not (red? (:l h))))
            (rotate-left h)
            h)
        h (if (and (red? (:l h)) (red? (:l (:l h))))
            (rotate-right h)
            h)
        h (if (and (red? (:l h)) (red? (:r h)))
            (flip-colors h)
            h)]
    h))

(defn move-red-left
  "Ensure (:l h) or one of its children is red."
  [h]
  (let [h (flip-colors h)]
    (if (red? (:l (:r h)))
      (let [h (assoc h :r (rotate-right (:r h)))
            h (rotate-left h)]
        (flip-colors h))
      h)))

(defn move-red-right
  "Ensure (:r h) or one of its children is red."
  [h]
  (let [h (flip-colors h)]
    (if (red? (:l (:l h)))
      (let [h (rotate-right h)]
        (flip-colors h))
      h)))

(defn rb-search
  "True when key k is present in tree h."
  [h k]
  (loop [cur h]
    (if (nil? cur)
      false
      (let [ck (:k cur)]
        (cond
          (= k ck) true
          (< k ck) (recur (:l cur))
          :else (recur (:r cur)))))))

(defn insert-node
  "Insert key k into subtree h."
  [h k]
  (if (nil? h)
    {:k k :c :red :l nil :r nil}
    (let [ck (:k h)
          h (cond
              (< k ck) (assoc h :l (insert-node (:l h) k))
              (> k ck) (assoc h :r (insert-node (:r h) k))
              :else h)]
      (balance h))))

(defn rb-insert
  "Insert key k into root and color root black."
  [root k]
  (assoc (insert-node root k) :c :black))

(defn min-node
  "Smallest node in non-empty subtree h."
  [h]
  (loop [cur h]
    (if (nil? (:l cur))
      cur
      (recur (:l cur)))))

(defn delete-min-node
  "Delete minimum node from subtree h."
  [h]
  (if (nil? (:l h))
    nil
    (let [h (if (and (not (red? (:l h))) (not (red? (:l (:l h)))))
              (move-red-left h)
              h)
          h (assoc h :l (delete-min-node (:l h)))]
      (balance h))))

(defn delete-node
  "Delete key k (known to be present) from subtree h."
  [h k]
  (if (< k (:k h))
    (let [h (if (and (not (red? (:l h))) (not (red? (:l (:l h)))))
              (move-red-left h)
              h)
          h (assoc h :l (delete-node (:l h) k))]
      (balance h))
    (let [h (if (red? (:l h))
              (rotate-right h)
              h)]
      (if (and (= k (:k h)) (nil? (:r h)))
        nil
        (let [h (if (and (not (red? (:r h))) (not (red? (:l (:r h)))))
                  (move-red-right h)
                  h)
              h (if (= k (:k h))
                  (let [m (min-node (:r h))]
                    (assoc h :k (:k m) :r (delete-min-node (:r h))))
                  (assoc h :r (delete-node (:r h) k)))]
          (balance h))))))

(defn rb-delete
  "Delete key k from root when present."
  [root k]
  (if (not (rb-search root k))
    root
    (let [root (if (and (not (red? (:l root))) (not (red? (:r root))))
                 (assoc root :c :red)
                 root)
          root (delete-node root k)]
      (when (some? root)
        (assoc root :c :black)))))

(defn rb-predecessor
  "Largest key strictly less than k, or nil."
  [root k]
  (loop [cur root ans nil]
    (if (nil? cur)
      ans
      (let [ck (:k cur)]
        (if (< ck k)
          (recur (:r cur) ck)
          (recur (:l cur) ans))))))

(defn rb-successor
  "Smallest key strictly greater than k, or nil."
  [root k]
  (loop [cur root ans nil]
    (if (nil? cur)
      ans
      (let [ck (:k cur)]
        (if (> ck k)
          (recur (:l cur) ck)
          (recur (:r cur) ans))))))

(defn run-ops
  "Execute q operations from toks, returning output lines or nil on invalid input."
  [toks q]
  (let [nt (count toks)]
    (loop [i 0 ti 1 root nil out []]
      (if (= i q)
        out
        (if (>= (inc ti) nt)
          nil
          (let [op (parse-num (nth toks ti))
                x (parse-num (nth toks (inc ti)))]
            (if (or (nil? op) (nil? x))
              nil
              (cond
                (= op 1)
                (recur (inc i) (+ ti 2) (rb-insert root x) out)

                (= op 2)
                (recur (inc i) (+ ti 2) (rb-delete root x) out)

                (= op 3)
                (recur (inc i) (+ ti 2) root
                       (conj out (if (rb-search root x) "1" "0")))

                (= op 4)
                (let [p (rb-predecessor root x)]
                  (recur (inc i) (+ ti 2) root
                         (conj out (if (nil? p) "NONE" (str p)))))

                (= op 5)
                (let [s (rb-successor root x)]
                  (recur (inc i) (+ ti 2) root
                         (conj out (if (nil? s) "NONE" (str s)))))

                :else nil))))))))

(defn -main
  "Run the red-black tree script and print one line per query op."
  [input]
  (let [toks (tokenize input)
        q (parse-num (first toks))]
    (if (or (nil? q) (neg? q))
      (fail "invalid input: expected an op count q, then q operations")
      (let [lines (run-ops toks q)]
        (if (nil? lines)
          (fail "invalid input: malformed red-black tree operations")
          (emit-lines lines))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
