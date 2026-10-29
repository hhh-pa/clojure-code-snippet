(require 'clojure.string)

;; Optimal bounds: random levels give expected O(log n) search, insert and
;; delete in O(n) expected space, the classic skip list guarantee.

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

(def max-level
  "Tallest tower any node may grow."
  16)

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

(defn rand-level
  "Draw a tower height, returning [height rng]."
  [rng]
  (loop [lvl 0 rng rng]
    (let [rng (rand-next rng)]
      (if (and (odd? rng) (< lvl max-level))
        (recur (inc lvl) rng)
        [lvl rng]))))

(defn empty-skip
  "Fresh skip list with an empty head tower and zero nodes."
  []
  {:nodes [] :head (vec (repeat (inc max-level) nil)) :level 0})

(defn node-val
  "Value stored at node id."
  [skip id]
  (:v (nth (:nodes skip) id)))

(defn forward-at
  "Successor of id at lvl, or of the head tower when id is nil."
  [skip id lvl]
  (if (nil? id)
    (nth (:head skip) lvl)
    (nth (:fwd (nth (:nodes skip) id)) lvl)))

(defn walk-level
  "Rightmost id at lvl whose value stays below v, starting from id."
  [skip id lvl v]
  (loop [cur id]
    (let [nxt (forward-at skip cur lvl)]
      (if (and nxt (< (node-val skip nxt) v))
        (recur nxt)
        cur))))

(defn find-update
  "Predecessor id per level for v, from top down to level 0."
  [skip v]
  (loop [lvl (:level skip) cur nil acc (vec (repeat (inc max-level) nil))]
    (if (< lvl 0)
      acc
      (let [cur (walk-level skip cur lvl v)]
        (recur (dec lvl) cur (assoc acc lvl cur))))))

(defn skip-search
  "True when v sits in the list."
  [skip v]
  (let [update (find-update skip v)
        cand (forward-at skip (nth update 0) 0)]
    (and (not (nil? cand)) (= (node-val skip cand) v))))

(defn set-forward
  "Point id (or the head tower for nil) at nxt on lvl."
  [skip id lvl nxt]
  (if (nil? id)
    (assoc skip :head (assoc (:head skip) lvl nxt))
    (assoc-in skip [:nodes id :fwd lvl] nxt)))

(defn skip-insert
  "Insert v unless present, returning [skip rng]."
  [skip rng v]
  (let [update (find-update skip v)
        cand (forward-at skip (nth update 0) 0)]
    (if (and cand (= (node-val skip cand) v))
      [skip rng]
      (let [[rlvl rng] (rand-level rng)
            top (max rlvl (:level skip))
            new-id (count (:nodes skip))
            node {:v v :fwd (vec (repeat (inc rlvl) nil))}
            skip (assoc skip
                        :nodes (conj (:nodes skip) node)
                        :level top)]
        [(loop [lvl 0 skip skip]
           (if (> lvl rlvl)
             skip
             (let [pred (nth update lvl)
                   nxt (forward-at skip pred lvl)
                   skip (set-forward skip new-id lvl nxt)
                   skip (set-forward skip pred lvl new-id)]
               (recur (inc lvl) skip))))
         rng]))))

(defn skip-delete
  "Remove v when present."
  [skip v]
  (let [update (find-update skip v)
        target (forward-at skip (nth update 0) 0)]
    (if (or (nil? target) (not= (node-val skip target) v))
      skip
      (let [skip (loop [lvl 0 skip skip]
                   (if (> lvl (:level skip))
                     skip
                     (let [pred (nth update lvl)]
                       (if (= (forward-at skip pred lvl) target)
                         (recur (inc lvl)
                                (set-forward skip pred lvl
                                             (forward-at skip target lvl)))
                         (recur (inc lvl) skip)))))]
        (loop [skip skip]
          (if (and (pos? (:level skip))
                   (nil? (forward-at skip nil (:level skip))))
            (recur (assoc skip :level (dec (:level skip))))
            skip))))))

(defn skip-line
  "Render the level 0 chain on one sorted line."
  [skip]
  (loop [cur (forward-at skip nil 0) acc []]
    (if (nil? cur)
      (clojure.string/join " " acc)
      (recur (forward-at skip cur 0) (conj acc (str (node-val skip cur)))))))

(defn run-ops
  "Execute q list ops from toks, returning lines or nil when invalid."
  [toks q]
  (let [nt (count toks)]
    (loop [i 0 ti 1 skip (empty-skip) rng 42 out []]
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
                  (let [[skip rng] (skip-insert skip rng v)]
                    (recur (inc i) (+ ti 2) skip rng out)))))

            (= op "SEARCH")
            (if (>= (inc ti) nt)
              nil
              (let [v (parse-num (nth toks (inc ti)))]
                (if (nil? v)
                  nil
                  (recur (inc i) (+ ti 2) skip rng
                         (conj out (if (skip-search skip v) "FOUND" "NOT_FOUND"))))))

            (= op "DELETE")
            (if (>= (inc ti) nt)
              nil
              (let [v (parse-num (nth toks (inc ti)))]
                (if (nil? v)
                  nil
                  (recur (inc i) (+ ti 2) (skip-delete skip v) rng out))))

            (= op "PRINT")
            (recur (inc i) (inc ti) skip rng (conj out (skip-line skip)))

            :else
            (recur (inc i) (inc ti) skip rng out)))))))

(defn -main
  "Run the skip list script and print one line per query op."
  [input]
  (let [toks (tokenize input)
        q (parse-num (first toks))]
    (if (or (nil? q) (neg? q))
      (fail "invalid input: expected an op count q, then q list ops")
      (let [lines (run-ops toks q)]
        (if (nil? lines)
          (fail "invalid input: list ops need integer values")
          (emit-lines lines))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
