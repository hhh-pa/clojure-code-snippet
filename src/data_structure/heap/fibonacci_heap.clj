(require 'clojure.string)

;; Optimal bounds: this is the classic Fibonacci heap, so insert and
;; decrease-key cost O(1) amortized while extract-min costs O(log n)
;; amortized, all in O(n) space. The node store below uses transients,
;; which are core Clojure cells with O(1) reads and writes.

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

(defn fresh-store
  "Transient node store with room for cap entries, plus an id lookup."
  [cap]
  {:ids (transient (vec (repeat cap 0)))
   :keys-vec (transient (vec (repeat cap 0)))
   :degs (transient (vec (repeat cap 0)))
   :marks (transient (vec (repeat cap false)))
   :parents (transient (vec (repeat cap -1)))
   :childs (transient (vec (repeat cap -1)))
   :lefts (transient (vec (repeat cap -1)))
   :rights (transient (vec (repeat cap -1)))
   :by-id (transient {})})

(defn insert-root
  "Splice idx into the root ring and return the new minimum index."
  [{:keys [keys-vec lefts rights]} idx cur-min]
  (if (= cur-min -1)
    (do
      (assoc! lefts idx idx)
      (assoc! rights idx idx)
      idx)
    (let [min-left (nth lefts cur-min)]
      (assoc! rights idx cur-min)
      (assoc! lefts idx min-left)
      (assoc! rights min-left idx)
      (assoc! lefts cur-min idx)
      (if (< (nth keys-vec idx) (nth keys-vec cur-min)) idx cur-min))))

(defn link-roots
  "Detach y from the root ring and adopt it as a child of x."
  [{:keys [parents childs lefts rights degs marks]} y x]
  (let [yl (nth lefts y)
        yr (nth rights y)]
    (assoc! rights yl yr)
    (assoc! lefts yr yl))
  (assoc! parents y x)
  (let [ch (nth childs x)]
    (if (= ch -1)
      (do
        (assoc! childs x y)
        (assoc! lefts y y)
        (assoc! rights y y))
      (let [chl (nth lefts ch)]
        (assoc! rights y ch)
        (assoc! lefts y chl)
        (assoc! rights chl y)
        (assoc! lefts ch y))))
  (assoc! degs x (inc (nth degs x)))
  (assoc! marks y false))

(defn consolidate
  "Merge same-degree roots until degrees are unique; return the new minimum."
  [{:keys [degs keys-vec lefts rights] :as store} cur-min]
  (if (= cur-min -1)
    -1
    (let [slots (transient (vec (repeat 50 -1)))
          roots (loop [cur cur-min acc []]
                  (let [acc (conj acc cur)
                        nxt (nth rights cur)]
                    (if (= nxt cur-min) acc (recur nxt acc))))]
      (loop [rs roots]
        (when (seq rs)
          (let [w (first rs)]
            (loop [x w d (nth degs x)]
              (if (and (< d 50) (not= (nth slots d) -1))
                (let [y (nth slots d)
                      nx (if (> (nth keys-vec x) (nth keys-vec y)) y x)
                      ny (if (> (nth keys-vec x) (nth keys-vec y)) x y)]
                  (link-roots store ny nx)
                  (assoc! slots d -1)
                  (recur nx (inc d)))
                (when (< d 50)
                  (assoc! slots d x)))))
          (recur (rest rs))))
      (loop [i 0 best -1]
        (if (>= i 50)
          best
          (let [nd (nth slots i)]
            (if (= nd -1)
              (recur (inc i) best)
              (if (= best -1)
                (do
                  (assoc! lefts nd nd)
                  (assoc! rights nd nd)
                  (recur (inc i) nd))
                (let [best-left (nth lefts best)]
                  (assoc! rights nd best)
                  (assoc! lefts nd best-left)
                  (assoc! rights best-left nd)
                  (assoc! lefts best nd)
                  (recur (inc i) (if (< (nth keys-vec nd) (nth keys-vec best))
                                   nd
                                   best)))))))))))

(defn extract-min
  "Remove the minimum root, returning [id key new-min store] or nil when empty.
   The store comes back because the id lookup may change identity on removal."
  [{:keys [ids keys-vec parents childs lefts rights by-id] :as store} cur-min]
  (when (not= cur-min -1)
    (let [z cur-min
          res-id (nth ids z)
          res-key (nth keys-vec z)
          ch (nth childs z)]
      (when (not= ch -1)
        (let [kids (loop [cur ch acc []]
                     (let [acc (conj acc cur)
                           nxt (nth rights cur)]
                       (if (= nxt ch) acc (recur nxt acc))))]
          (loop [ks kids]
            (when (seq ks)
              (let [c (first ks)
                    z-left (nth lefts z)]
                (assoc! rights c z)
                (assoc! lefts c z-left)
                (assoc! rights z-left c)
                (assoc! lefts z c)
                (assoc! parents c -1))
              (recur (rest ks))))))
      (let [zl (nth lefts z)
            zr (nth rights z)]
        (assoc! rights zl zr)
        (assoc! lefts zr zl)
        (let [best (if (= z zr) -1 (consolidate store zr))
              store (assoc store :by-id (dissoc! by-id res-id))]
          [res-id res-key best store])))))

(defn cut-node
  "Detach x from its parent y and move x back to the root ring."
  [{:keys [parents childs lefts rights degs marks]} x y cur-min]
  (if (= (nth rights x) x)
    (assoc! childs y -1)
    (let [xl (nth lefts x)
          xr (nth rights x)]
      (assoc! rights xl xr)
      (assoc! lefts xr xl)
      (when (= (nth childs y) x)
        (assoc! childs y xr))))
  (assoc! degs y (dec (nth degs y)))
  (let [min-left (nth lefts cur-min)]
    (assoc! rights x cur-min)
    (assoc! lefts x min-left)
    (assoc! rights min-left x)
    (assoc! lefts cur-min x)
    (assoc! parents x -1)
    (assoc! marks x false)))

(defn cascading-cut
  "Cut y when already marked, propagating upward while marks persist."
  [{:keys [parents marks] :as store} y cur-min]
  (let [z (nth parents y)]
    (when (not= z -1)
      (if-not (nth marks y)
        (assoc! marks y true)
        (do
          (cut-node store y z cur-min)
          (cascading-cut store z cur-min))))))

(defn decrease-key
  "Lower the key of id, cutting and cascading as needed; return new minimum."
  [{:keys [keys-vec parents by-id] :as store} id new-key cur-min]
  (let [idx (get by-id id -1)]
    (if (< idx 0)
      cur-min
      (if (> new-key (nth keys-vec idx))
        cur-min
        (do
          (assoc! keys-vec idx new-key)
          (let [y (nth parents idx)]
            (when (and (not= y -1) (< (nth keys-vec idx) (nth keys-vec y)))
              (cut-node store idx y cur-min)
              (cascading-cut store y cur-min)))
          (if (and (not= cur-min -1) (< (nth keys-vec idx) (nth keys-vec cur-min)))
            idx
            cur-min))))))

(defn run-ops
  "Execute q heap ops from toks, returning output lines or nil when invalid.
   The store rides the loop because assoc on the id lookup may hand back a
   promoted lookup object, which must replace the old one."
  [toks q]
  (let [nt (count toks)]
    ;; Each op needs at least its name token; more ops than remaining
    ;; tokens is malformed. This also bounds the store by input size.
    (if (> q (dec nt))
      nil
      (loop [i 0 ti 1 cur-min -1 cnt 0 store (fresh-store (max 10 q)) out []]
      (if (or (>= i q) (>= ti nt))
        out
        (let [op (nth toks ti)]
          (cond
            (= op "INSERT")
            (if (>= (+ ti 2) nt)
              nil
              (let [id (parse-num (nth toks (+ ti 1)))
                    val (parse-num (nth toks (+ ti 2)))]
                (if (or (nil? id) (nil? val))
                  nil
                  (let [idx cnt
                        {:keys [ids keys-vec degs marks parents childs by-id]} store]
                    (assoc! ids idx id)
                    (assoc! keys-vec idx val)
                    (assoc! degs idx 0)
                    (assoc! marks idx false)
                    (assoc! parents idx -1)
                    (assoc! childs idx -1)
                    (let [store (assoc store :by-id (assoc! by-id id idx))]
                      (recur (inc i) (+ ti 3) (insert-root store idx cur-min)
                             (inc cnt) store out))))))

            (= op "MIN")
            (recur (inc i) (inc ti) cur-min cnt store
                   (conj out (if (= cur-min -1)
                               "EMPTY"
                               (str (nth (:ids store) cur-min)
                                    " "
                                    (nth (:keys-vec store) cur-min)))))

            (= op "EXTRACT_MIN")
            (let [res (extract-min store cur-min)]
              (if res
                (recur (inc i) (inc ti) (nth res 2) cnt (nth res 3)
                       (conj out (str (nth res 0) " " (nth res 1))))
                (recur (inc i) (inc ti) cur-min cnt store (conj out "EMPTY"))))

            (= op "DECREASE_KEY")
            (if (>= (+ ti 2) nt)
              nil
              (let [id (parse-num (nth toks (+ ti 1)))
                    val (parse-num (nth toks (+ ti 2)))]
                (if (or (nil? id) (nil? val))
                  nil
                  (recur (inc i) (+ ti 3)
                         (decrease-key store id val cur-min) cnt store out))))

            :else
            (recur (inc i) (inc ti) cur-min cnt store out))))))))

(defn -main
  "Run the heap script and print one line per query op."
  [input]
  (let [toks (tokenize input)
        q (parse-num (first toks))]
    (if (or (nil? q) (neg? q))
      (fail "invalid input: expected an op count q, then q heap ops")
      (let [lines (run-ops toks q)]
        (if (nil? lines)
          (fail "invalid input: heap ops need integer ids and keys")
          (emit-lines lines))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
