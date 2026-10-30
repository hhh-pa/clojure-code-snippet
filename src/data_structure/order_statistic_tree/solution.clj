(require 'clojure.string)

;; Optimal bounds: Order-Statistic Tree (Red-Black Tree augmented with subtree
;; sizes) supports OS-Insert, OS-Delete, OS-Select, and OS-Rank in worst-case
;; O(log n) time and O(n) space, and solves the (n, m)-Josephus problem in
;; O(n log n) time.

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

(defn node-size
  "Subtree size of h."
  [h]
  (if (nil? h) 0 (:sz h)))

(defn update-size
  "Recompute subtree size of h."
  [h]
  (assoc h :sz (+ 1 (node-size (:l h)) (node-size (:r h)))))

(defn flip-color
  "Toggle :red and :black."
  [c]
  (if (= c :red) :black :red))

(defn rotate-left
  "Left-rotate red right link of h, maintaining subtree sizes."
  [h]
  (let [x (:r h)
        h2 (update-size (assoc h :r (:l x) :c :red))]
    (assoc x :l h2 :c (:c h) :sz (:sz h))))

(defn rotate-right
  "Right-rotate red left link of h, maintaining subtree sizes."
  [h]
  (let [x (:l h)
        h2 (update-size (assoc h :l (:r x) :c :red))]
    (assoc x :r h2 :c (:c h) :sz (:sz h))))

(defn flip-colors
  "Flip colors of h and its two children."
  [h]
  (assoc h
         :c (flip-color (:c h))
         :l (assoc (:l h) :c (flip-color (:c (:l h))))
         :r (assoc (:r h) :c (flip-color (:c (:r h))))))

(defn balance
  "Restore red-black invariants and subtree size at node h."
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
    (update-size h)))

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

(defn os-contains?
  "True when key k is present in tree root."
  [root k]
  (loop [cur root]
    (if (nil? cur)
      false
      (let [ck (:k cur)]
        (cond
          (= k ck) true
          (< k ck) (recur (:l cur))
          :else (recur (:r cur)))))))

(defn insert-node
  "Insert distinct key k into subtree h."
  [h k]
  (if (nil? h)
    {:k k :c :red :sz 1 :l nil :r nil}
    (let [ck (:k h)
          h (cond
              (< k ck) (assoc h :l (insert-node (:l h) k))
              (> k ck) (assoc h :r (insert-node (:r h) k))
              :else h)]
      (balance h))))

(defn os-insert
  "Insert key k into root and color root black."
  [root k]
  (if (os-contains? root k)
    root
    (assoc (insert-node root k) :c :black)))

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
                  (let [x (min-node (:r h))]
                    (assoc h :k (:k x) :r (delete-min-node (:r h))))
                  (assoc h :r (delete-node (:r h) k)))]
          (balance h))))))

(defn os-delete
  "Delete key k from root when present."
  [root k]
  (if (not (os-contains? root k))
    root
    (let [root (if (and (not (red? (:l root))) (not (red? (:r root))))
                 (assoc root :c :red)
                 root)
          root (delete-node root k)]
      (when (some? root)
        (assoc root :c :black)))))

(defn os-select
  "1-based k-th smallest key in root, or nil when out of bounds."
  [root k]
  (if (or (< k 1) (> k (node-size root)))
    nil
    (loop [cur root i k]
      (let [r (inc (node-size (:l cur)))]
        (cond
          (= i r) (:k cur)
          (< i r) (recur (:l cur) i)
          :else (recur (:r cur) (- i r)))))))

(defn os-rank
  "Count of keys in root strictly less than x."
  [root x]
  (loop [cur root ans 0]
    (if (nil? cur)
      ans
      (let [ck (:k cur)]
        (if (<= x ck)
          (recur (:l cur) ans)
          (recur (:r cur) (+ ans 1 (node-size (:l cur)))))))))

(defn josephus-permutation
  "Compute the (n, m)-Josephus elimination order of 1..n in O(n log n)."
  [n m]
  (let [init-tree (reduce os-insert nil (range 1 (inc n)))]
    (loop [rem-cnt n idx 0 tree init-tree acc []]
      (if (zero? rem-cnt)
        acc
        (let [next-idx (rem (+ idx m -1) rem-cnt)
              person (os-select tree (inc next-idx))]
          (recur (dec rem-cnt)
                 next-idx
                 (os-delete tree person)
                 (conj acc person)))))))

(defn run-ops
  "Execute q order-statistic tree ops from toks, returning lines or nil on invalid input."
  [toks q]
  (let [nt (count toks)]
    (loop [i 0 ti 1 root nil out []]
      (if (= i q)
        out
        (if (>= ti nt)
          nil
          (let [op (parse-num (nth toks ti))]
            (cond
              (= op 1)
              (if (>= (inc ti) nt)
                nil
                (let [x (parse-num (nth toks (inc ti)))]
                  (if (nil? x)
                    nil
                    (recur (inc i) (+ ti 2) (os-insert root x) out))))

              (= op 2)
              (if (>= (inc ti) nt)
                nil
                (let [x (parse-num (nth toks (inc ti)))]
                  (if (nil? x)
                    nil
                    (recur (inc i) (+ ti 2) (os-delete root x) out))))

              (= op 3)
              (if (>= (inc ti) nt)
                nil
                (let [k (parse-num (nth toks (inc ti)))]
                  (if (nil? k)
                    nil
                    (let [v (os-select root k)]
                      (recur (inc i) (+ ti 2) root
                             (conj out (if (nil? v) "NONE" (str v))))))))

              (= op 4)
              (if (>= (inc ti) nt)
                nil
                (let [x (parse-num (nth toks (inc ti)))]
                  (if (nil? x)
                    nil
                    (recur (inc i) (+ ti 2) root
                           (conj out (str (os-rank root x)))))))

              (= op 5)
              (if (>= (+ ti 2) nt)
                nil
                (let [n (parse-num (nth toks (inc ti)))
                      m (parse-num (nth toks (+ ti 2)))]
                  (if (or (nil? n) (nil? m) (< n 1) (< m 1))
                    nil
                    (let [perm (josephus-permutation n m)]
                      (recur (inc i) (+ ti 3) root
                             (conj out (clojure.string/join " " (map str perm))))))))

              :else nil)))))))

(defn -main
  "Run the order-statistic tree script and print one line per query op."
  [input]
  (let [toks (tokenize input)
        q (parse-num (first toks))]
    (if (or (nil? q) (neg? q))
      (fail "invalid input: expected an op count q, then q order-statistic ops")
      (let [lines (run-ops toks q)]
        (if (nil? lines)
          (fail "invalid input: malformed order-statistic tree operations")
          (emit-lines lines))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
