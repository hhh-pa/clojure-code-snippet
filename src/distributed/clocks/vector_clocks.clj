;; Vector clock simulation with causality queries.
;; Reads num-procs num-ops, then LOCAL p name / SEND p msg / RECV p msg ops,
;; then num-queries, then event name pairs.
;; Time is O((ops + queries) P) and stored event copies use O(events P) space,
;; which is optimal because each op and each query must inspect P entries.
;; Blank input prints nothing. Invalid input prints a note to stderr and
;; returns normally with no stdout. Exit stays 0 since a nonzero exit would
;; need host interop, which is retired.

(require '[clojure.string :as str])

(def digit-values
  "Map from digit char to its numeric value."
  (zipmap "0123456789" (range)))

(defn whitespace?
  "True for ASCII whitespace chars."
  [ch]
  (<= (compare ch \space) 0))

(defn tokenize
  "Split string s on ASCII whitespace into a vector of token strings."
  [s]
  (let [[toks cur] (reduce (fn [[toks cur] ch]
                             (if (whitespace? ch)
                               (if (seq cur)
                                 [(conj toks (str/join cur)) []]
                                 [toks cur])
                               [toks (conj cur ch)]))
                           [[] []]
                           (seq s))]
    (if (seq cur)
      (conj toks (str/join cur))
      toks)))

(defn parse-int
  "Parse an optional-sign decimal token into an integer, else nil."
  [tok]
  (when (seq tok)
    (let [chs (seq tok)
          [neg? ds] (cond (= (first chs) \-) [true (next chs)]
                          (= (first chs) \+) [false (next chs)]
                          :else [false chs])]
      (when (seq ds)
        (let [mag (reduce (fn [acc ch]
                            (when (some? acc)
                              (let [d (get digit-values ch nil)]
                                (when (some? d)
                                  (+' (*' acc 10) d)))))
                          0
                          ds)]
          (when (some? mag)
            (if neg? (-' mag) mag)))))))

(defn fail!
  "Print msg to stderr and return nil. Stdout stays empty and exit stays 0."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn zero-clock
  "Fresh all-zero clock for p processes."
  [p]
  (vec (repeat p 0)))

(defn tick
  "Increment the p-th component of clock v."
  [v p]
  (assoc v p (inc (nth v p))))

(defn merge-clocks
  "Component-wise maximum of clocks a and b."
  [a b]
  (mapv max a b))

(defn clock-relation
  "Relate clocks a and b: :eq when equal, :lt when a precedes b, :gt when
   b precedes a, :concurrent when neither precedes the other."
  [a b]
  (let [aleb (every? true? (map <= a b))
        blea (every? true? (map <= b a))]
    (cond (and aleb blea) :eq
          aleb :lt
          blea :gt
          :else :concurrent)))

(defn query-line
  "One causality line for events e1 and e2 with clocks v1 and v2."
  [e1 e2 v1 v2]
  (case (clock-relation v1 v2)
    :lt (str e1 " -> " e2)
    :gt (str e2 " -> " e1)
    :eq (str e1 " == " e2)
    (str e1 " || " e2)))

(defn valid-proc?
  "True when p names one of n processes."
  [n p]
  (and (some? p) (integer? p) (<= 0 p) (< p n)))

(defn read-ops
  "Read n-ops ops from toks starting at ti. Returns [ops ti] or fails."
  [toks ti n-ops n-procs]
  (loop [i 0, ti ti, ops []]
    (if (>= i n-ops)
      [ops ti]
      (if (>= ti (count toks))
        (fail! "Invalid input: fewer op tokens than num-ops claims")
        (let [op (nth toks ti)]
          (cond
            (= op "LOCAL")
            (if (>= (+ ti 2) (count toks))
              (fail! "Invalid input: LOCAL needs a process and a name")
              (let [p (parse-int (nth toks (+ ti 1)))]
                (if-not (valid-proc? n-procs p)
                  (fail! "Invalid input: LOCAL process out of range")
                  (recur (inc i) (+ ti 3)
                         (conj ops [:local p (nth toks (+ ti 2))])))))
            (= op "SEND")
            (if (>= (+ ti 2) (count toks))
              (fail! "Invalid input: SEND needs a process and a message id")
              (let [p (parse-int (nth toks (+ ti 1)))]
                (if-not (valid-proc? n-procs p)
                  (fail! "Invalid input: SEND process out of range")
                  (recur (inc i) (+ ti 3)
                         (conj ops [:send p (nth toks (+ ti 2))])))))
            (= op "RECV")
            (if (>= (+ ti 2) (count toks))
              (fail! "Invalid input: RECV needs a process and a message id")
              (let [p (parse-int (nth toks (+ ti 1)))]
                (if-not (valid-proc? n-procs p)
                  (fail! "Invalid input: RECV process out of range")
                  (recur (inc i) (+ ti 3)
                         (conj ops [:recv p (nth toks (+ ti 2))])))))
            :else
            (fail! (str "Invalid input: unknown op " op))))))))

(defn apply-op
  "Apply one parsed op to state, returning the next state."
  [{:keys [vc events in-flight] :as state} op]
  (cond
    (= (first op) :local)
    (let [[_ p label] op
          v (tick (nth vc p) p)]
      (assoc state :vc (assoc vc p v) :events (assoc events label v)))
    (= (first op) :send)
    (let [[_ p msg] op
          v (tick (nth vc p) p)]
      (assoc state
             :vc (assoc vc p v)
             :in-flight (assoc in-flight msg v)
             :events (assoc events (str msg "_SEND") v)))
    :else
    (let [[_ p msg] op
          cur (nth vc p)
          got (get in-flight msg)
          v (tick (if (some? got) (merge-clocks cur got) cur) p)]
      (assoc state
             :vc (assoc vc p v)
             :events (assoc events (str msg "_RECV") v)))))

(defn run-queries
  "Answer the query block starting at ti, returning lines or failing."
  [toks ti events n-procs]
  (if (>= ti (count toks))
    []
    (let [nq (parse-int (nth toks ti))]
      (cond
        (or (nil? nq) (neg? nq))
        (fail! "Invalid input: num-queries must be a non-negative integer")
        (< (count toks) (+' ti 1 (*' 2 nq)))
        (fail! "Invalid input: fewer query tokens than num-queries claims")
        :else
        (let [zero (zero-clock n-procs)]
          (vec (for [q (range nq)
                     :let [e1 (nth toks (+ ti 1 (* 2 q)))
                           e2 (nth toks (+ ti 2 (* 2 q)))
                           v1 (get events e1 zero)
                           v2 (get events e2 zero)]]
                 (query-line e1 e2 v1 v2))))))))

(defn simulate
  "Build output lines from toks, or fail to stderr and return nil."
  [toks]
  (if (< (count toks) 2)
    (fail! "Invalid input: expected num-procs num-ops ...")
    (let [n-procs (parse-int (nth toks 0))
          n-ops (parse-int (nth toks 1))]
      (cond
        (or (nil? n-procs) (neg? n-procs))
        (fail! "Invalid input: num-procs must be a non-negative integer")
        (or (nil? n-ops) (neg? n-ops))
        (fail! "Invalid input: num-ops must be a non-negative integer")
        :else
        (let [res (read-ops toks 2 n-ops n-procs)]
          (when (some? res)
            (let [[ops ti] res
                  init {:vc (vec (repeat n-procs (zero-clock n-procs)))
                        :events {}
                        :in-flight {}}
                  state (reduce apply-op init ops)]
              (run-queries toks ti (:events state) n-procs))))))))

(defn -main
  "Entry point: tokenize INPUT and print result lines. Blank input prints
   nothing. Invalid input prints a note to stderr with no stdout."
  [input]
  (when-not (str/blank? input)
    (let [lines (simulate (tokenize input))]
      (when (some? lines)
        (run! println lines)))))

(-main (slurp *in*))
