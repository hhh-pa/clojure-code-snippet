(require 'clojure.string)

;; Optimal bounds: Sedgewick's 2-3 Left-Leaning Red-Black BST guarantees
;; worst-case O(log n) put, delete, get, range-count, and floor in O(n) space.

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
  "Make a right-leaning red link lean left."
  [h]
  (let [x (:r h)
        h2 (update-size (assoc h :r (:l x) :c :red))]
    (assoc x :l h2 :c (:c h) :sz (:sz h))))

(defn rotate-right
  "Make a left-leaning red link temporarily lean right."
  [h]
  (let [x (:l h)
        h2 (update-size (assoc h :l (:r x) :c :red))]
    (assoc x :r h2 :c (:c h) :sz (:sz h))))

(defn flip-colors
  "Flip the colors of node h and its two children."
  [h]
  (assoc h
         :c (flip-color (:c h))
         :l (assoc (:l h) :c (flip-color (:c (:l h))))
         :r (assoc (:r h) :c (flip-color (:c (:r h))))))

(defn balance
  "Restore LLRB invariants and subtree size at node h."
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
  "Assuming h is red and both (:l h) and (:l (:l h)) are black, make (:l h) or one of its children red."
  [h]
  (let [h (flip-colors h)]
    (if (red? (:l (:r h)))
      (let [h (assoc h :r (rotate-right (:r h)))
            h (rotate-left h)]
        (flip-colors h))
      h)))

(defn move-red-right
  "Assuming h is red and both (:r h) and (:l (:r h)) are black, make (:r h) or one of its children red."
  [h]
  (let [h (flip-colors h)]
    (if (red? (:l (:l h)))
      (let [h (rotate-right h)]
        (flip-colors h))
      h)))

(defn llrb-get
  "Return value associated with key k, or nil when absent."
  [root k]
  (loop [cur root]
    (if (nil? cur)
      nil
      (let [ck (:k cur)]
        (cond
          (= k ck) (:v cur)
          (< k ck) (recur (:l cur))
          :else (recur (:r cur)))))))

(defn put-node
  "Insert or update key k with value v in subtree h."
  [h k v]
  (if (nil? h)
    {:k k :v v :c :red :sz 1 :l nil :r nil}
    (let [ck (:k h)
          h (cond
              (< k ck) (assoc h :l (put-node (:l h) k v))
              (> k ck) (assoc h :r (put-node (:r h) k v))
              :else (assoc h :v v))]
      (balance h))))

(defn llrb-put
  "Put key k with value v into root and color root black."
  [root k v]
  (assoc (put-node root k v) :c :black))

(defn min-node
  "Minimum node in non-empty subtree h."
  [h]
  (loop [cur h]
    (if (nil? (:l cur))
      cur
      (recur (:l cur)))))

(defn delete-min
  "Delete minimum key-value pair from subtree h."
  [h]
  (if (nil? (:l h))
    nil
    (let [h (if (and (not (red? (:l h))) (not (red? (:l (:l h)))))
              (move-red-left h)
              h)
          h (assoc h :l (delete-min (:l h)))]
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
                    (assoc h :k (:k x) :v (:v x) :r (delete-min (:r h))))
                  (assoc h :r (delete-node (:r h) k)))]
          (balance h))))))

(defn llrb-delete
  "Delete key k from root when present."
  [root k]
  (if (nil? (llrb-get root k))
    root
    (let [root (if (and (not (red? (:l root))) (not (red? (:r root))))
                 (assoc root :c :red)
                 root)
          root (delete-node root k)]
      (when (some? root)
        (assoc root :c :black)))))

(defn llrb-rank
  "Count of keys strictly less than k."
  [root k]
  (loop [cur root ans 0]
    (if (nil? cur)
      ans
      (let [ck (:k cur)]
        (cond
          (< k ck) (recur (:l cur) ans)
          (> k ck) (recur (:r cur) (+ ans 1 (node-size (:l cur))))
          :else (+ ans (node-size (:l cur))))))))

(defn llrb-range-count
  "Count of keys in [lo, hi]."
  [root lo hi]
  (if (> lo hi)
    0
    (+ (- (llrb-rank root hi) (llrb-rank root lo))
       (if (some? (llrb-get root hi)) 1 0))))

(defn llrb-floor
  "Largest key <= k, or nil."
  [root k]
  (loop [cur root ans nil]
    (if (nil? cur)
      ans
      (let [ck (:k cur)]
        (cond
          (= k ck) ck
          (< k ck) (recur (:l cur) ans)
          :else (recur (:r cur) ck))))))

(defn run-ops
  "Execute q LLRB ops from toks, returning output lines or nil on invalid input."
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
              (if (>= (+ ti 2) nt)
                nil
                (let [k (parse-num (nth toks (inc ti)))
                      v (parse-num (nth toks (+ ti 2)))]
                  (if (or (nil? k) (nil? v))
                    nil
                    (recur (inc i) (+ ti 3) (llrb-put root k v) out))))

              (= op 2)
              (if (>= (inc ti) nt)
                nil
                (let [k (parse-num (nth toks (inc ti)))]
                  (if (nil? k)
                    nil
                    (recur (inc i) (+ ti 2) (llrb-delete root k) out))))

              (= op 3)
              (if (>= (inc ti) nt)
                nil
                (let [k (parse-num (nth toks (inc ti)))]
                  (if (nil? k)
                    nil
                    (let [v (llrb-get root k)]
                      (recur (inc i) (+ ti 2) root
                             (conj out (if (nil? v) "NONE" (str v))))))))

              (= op 4)
              (if (>= (+ ti 2) nt)
                nil
                (let [lo (parse-num (nth toks (inc ti)))
                      hi (parse-num (nth toks (+ ti 2)))]
                  (if (or (nil? lo) (nil? hi))
                    nil
                    (recur (inc i) (+ ti 3) root
                           (conj out (str (llrb-range-count root lo hi)))))))

              (= op 5)
              (if (>= (inc ti) nt)
                nil
                (let [k (parse-num (nth toks (inc ti)))]
                  (if (nil? k)
                    nil
                    (let [fk (llrb-floor root k)]
                      (recur (inc i) (+ ti 2) root
                             (conj out (if (nil? fk) "NONE" (str fk))))))))

              :else nil)))))))

(defn -main
  "Run the LLRB tree script and print one line per query op."
  [input]
  (let [toks (tokenize input)
        q (parse-num (first toks))]
    (if (or (nil? q) (neg? q))
      (fail "invalid input: expected an op count q, then q LLRB ops")
      (let [lines (run-ops toks q)]
        (if (nil? lines)
          (fail "invalid input: malformed LLRB tree operations")
          (emit-lines lines))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
