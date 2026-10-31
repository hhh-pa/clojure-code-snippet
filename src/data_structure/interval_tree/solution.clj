(require 'clojure.string)

;; Augmented Interval Tree (CLRS Ch 17.3): AVL-balanced BST keyed by [lo hi id]
;; and augmented with subtree max-hi. Supports insert and delete in O(log n)
;; time and overlap queries in O(min(n, (k + 1) log n) + k log k) time and
;; O(n) space.

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

(defn node-height
  "Return the AVL height of node u."
  [u]
  (if u (:h u) 0))

(defn make-node
  "Construct an interval tree node with recomputed height and max-hi."
  [k l r]
  (let [hi (nth k 1)
        m1 (if l (max hi (:max-hi l)) hi)
        m2 (if r (max m1 (:max-hi r)) m1)
        h (inc (max (node-height l) (node-height r)))]
    {:k k :max-hi m2 :h h :l l :r r}))

(defn rot-left
  "Single left rotation with augmented fields updated."
  [x]
  (let [y (:r x)
        nx (make-node (:k x) (:l x) (:l y))]
    (make-node (:k y) nx (:r y))))

(defn rot-right
  "Single right rotation with augmented fields updated."
  [y]
  (let [x (:l y)
        ny (make-node (:k y) (:r x) (:r y))]
    (make-node (:k x) (:l x) ny)))

(defn balance
  "Restore AVL balance and augmented fields for node with key k and children l, r."
  [k l r]
  (let [bf (- (node-height l) (node-height r))]
    (cond
      (> bf 1)
      (let [l2 (if (< (node-height (:l l)) (node-height (:r l)))
                 (rot-left l)
                 l)]
        (rot-right (make-node k l2 r)))

      (< bf -1)
      (let [r2 (if (< (node-height (:r r)) (node-height (:l r)))
                 (rot-right r)
                 r)]
        (rot-left (make-node k l r2)))

      :else
      (make-node k l r))))

(defn avl-insert
  "Insert key k = [lo hi id] into the augmented AVL tree."
  [u k]
  (if (nil? u)
    (make-node k nil nil)
    (let [c (compare k (:k u))]
      (cond
        (neg? c) (balance (:k u) (avl-insert (:l u) k) (:r u))
        (pos? c) (balance (:k u) (:l u) (avl-insert (:r u) k))
        :else u))))

(defn leftmost-key
  "Return the minimum key in non-nil subtree u."
  [u]
  (loop [cur u]
    (if (:l cur)
      (recur (:l cur))
      (:k cur))))

(defn avl-remove
  "Remove key k = [lo hi id] from the augmented AVL tree."
  [u k]
  (when u
    (let [c (compare k (:k u))]
      (cond
        (neg? c) (balance (:k u) (avl-remove (:l u) k) (:r u))
        (pos? c) (balance (:k u) (:l u) (avl-remove (:r u) k))
        (nil? (:l u)) (:r u)
        (nil? (:r u)) (:l u)
        :else
        (let [sk (leftmost-key (:r u))]
          (balance sk (:l u) (avl-remove (:r u) sk)))))))

(defn avl-query
  "Return a vector of ids of intervals in subtree u overlapping [qlo, qhi]."
  [u qlo qhi acc]
  (if (or (nil? u) (< (:max-hi u) qlo))
    acc
    (let [acc1 (avl-query (:l u) qlo qhi acc)
          [lo hi id] (:k u)
          acc2 (if (and (<= lo qhi) (<= qlo hi))
                 (conj acc1 id)
                 acc1)]
      (if (<= lo qhi)
        (avl-query (:r u) qlo qhi acc2)
        acc2))))

(defn run-ops
  "Execute q interval tree ops from toks, returning output lines or nil on error."
  [toks q]
  (let [nt (count toks)]
    (loop [i 0 ti 1 root nil by-id {} out []]
      (if (= i q)
        out
        (if (>= ti nt)
          nil
          (let [op (parse-num (nth toks ti))]
            (cond
              (= op 1)
              (if (>= (+ ti 3) nt)
                nil
                (let [lo (parse-num (nth toks (+ ti 1)))
                      hi (parse-num (nth toks (+ ti 2)))
                      id (parse-num (nth toks (+ ti 3)))]
                  (if (or (nil? lo) (nil? hi) (nil? id) (> lo hi) (<= id 0))
                    nil
                    (let [old (get by-id id)
                          r1 (if old (avl-remove root old) root)
                          nk [lo hi id]
                          r2 (avl-insert r1 nk)]
                      (recur (inc i) (+ ti 4) r2 (assoc by-id id nk) out)))))

              (= op 2)
              (if (>= (inc ti) nt)
                nil
                (let [id (parse-num (nth toks (inc ti)))]
                  (if (nil? id)
                    nil
                    (if-let [old (get by-id id)]
                      (recur (inc i) (+ ti 2) (avl-remove root old) (dissoc by-id id) out)
                      (recur (inc i) (+ ti 2) root by-id out)))))

              (= op 3)
              (if (>= (+ ti 2) nt)
                nil
                (let [qlo (parse-num (nth toks (+ ti 1)))
                      qhi (parse-num (nth toks (+ ti 2)))]
                  (if (or (nil? qlo) (nil? qhi) (> qlo qhi))
                    nil
                    (let [ids (sort (avl-query root qlo qhi []))
                          line (if (seq ids)
                                 (clojure.string/join " " (map str ids))
                                 "NONE")]
                      (recur (inc i) (+ ti 3) root by-id (conj out line))))))

              :else
              nil)))))))

(defn -main
  "Parse input and run the interval tree operations."
  [input]
  (let [toks (tokenize input)
        q (parse-num (first toks))]
    (if (or (nil? q) (neg? q))
      (fail "invalid input: expected non-negative operation count Q")
      (let [lines (run-ops toks q)]
        (if (nil? lines)
          (fail "invalid input: malformed interval tree operation")
          (emit-lines lines))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
