;; Tableau simplex for max c x with A x <= b and x >= 0.
;;
;; stdin shape: m n, then n costs, then m rows of n entries plus rhs.
;; stdout: OPTIMAL plus value with two decimals, else unbounded.
;; Uses most negative entering choice with min ratio leaving choice.
;; Each pivot costs rows times cols time and the flat tableau vector
;; is the only big store. Doubles parse and print by hand, so no host
;; calls stay. Blank stdin gives no output. Bad input gives a stderr
;; note and the script returns normally, so exit stays zero.

(require '[clojure.string :as str])

(def digit-values
  "Digit char to int value."
  (zipmap "0123456789" (range)))

(defn parse-int
  "Parse a signed decimal token with exact math."
  [tok]
  (let [neg (= \- (first tok))
        body (if neg (rest tok) (seq tok))]
    (let [v (reduce (fn [acc ch] (+' (*' acc 10) (digit-values ch)))
                    0
                    body)]
      (if neg (-' v) v))))

(defn int-token?
  "True when tok is an optionally signed run of decimal digits."
  [tok]
  (boolean
   (and (seq tok)
        (let [body (if (= \- (first tok)) (rest tok) (seq tok))]
          (and (seq body)
               (every? digit-values body))))))

(defn fail
  "Print message to stderr, then return normally with exit zero."
  [msg]
  (binding [*out* *err*]
    (println msg)))

(def eps
  "Zero tolerance, same value as the old script."
  1e-9)

(def ratio-inf
  "Starting best ratio, same value as the old script."
  1e18)

(defn abs-num
  "Absolute value with no host call."
  [x]
  (if (neg? x) (- x) x))

(defn pow10
  "10.0 raised to e, with e a non-negative int."
  [e]
  (loop [i 0
         acc 1.0]
    (if (>= i e)
      acc
      (recur (inc i) (* acc 10.0)))))

(defn find-char
  "First index of pred char at or after start, or nil."
  [chs pred start end]
  (loop [i start]
    (cond
      (>= i end) nil
      (pred (nth chs i)) i
      :else (recur (inc i)))))

(defn double-token?
  "True for sign, digits, dot, digits, and exponent number shapes."
  [tok]
  (boolean
   (and (seq tok)
        (let [chs (vec tok)
              n (count chs)
              s0 (if (or (= (nth chs 0) \-) (= (nth chs 0) \+)) 1 0)]
          (and (< s0 n)
               (let [eidx (find-char chs
                                     (fn [ch] (or (= ch \e) (= ch \E)))
                                     s0 n)
                     mend (or eidx n)
                     mant (subvec chs s0 mend)
                     dots (count (filter (fn [ch] (= ch \.)) mant))
                     digs (count (filter digit-values mant))]
                 (and (> mend s0)
                      (<= dots 1)
                      (>= digs 1)
                      (every? (fn [ch] (or (= ch \.) (digit-values ch))) mant)
                      (if eidx
                        (let [es (inc eidx)
                              ds (if (and (< es n)
                                          (or (= (nth chs es) \-)
                                              (= (nth chs es) \+)))
                                   (inc es)
                                   es)]
                          (and (< es n)
                               (< ds n)
                               (every? digit-values (subvec chs ds n))))
                        true))))))))

(defn parse-dec
  "Parse a decimal token into a double with no host call."
  [tok]
  (let [chs (vec tok)
        n (count chs)
        neg (= \- (nth chs 0))
        start (if (or neg (= \+ (nth chs 0))) 1 0)
        eidx (find-char chs (fn [ch] (or (= ch \e) (= ch \E))) start n)
        mend (or eidx n)
        didx (find-char chs (fn [ch] (= ch \.)) start mend)
        ipart (subvec chs start (or didx mend))
        fpart (when didx (subvec chs (inc didx) mend))
        ival (reduce (fn [a ch] (+ (* a 10.0) (digit-values ch))) 0.0 ipart)
        fval (if (seq fpart)
               (/ (reduce (fn [a ch] (+ (* a 10.0) (digit-values ch)))
                          0.0
                          fpart)
                  (pow10 (count fpart)))
               0.0)
        base (+ ival fval)
        exp-val (if eidx
                  (let [echs (subvec chs (inc eidx) n)
                        eneg (= \- (first echs))
                        digs (if (or eneg (= \+ (first echs)))
                               (rest echs)
                               (seq echs))
                        e (reduce (fn [a ch] (+ (* a 10) (digit-values ch)))
                                  0
                                  digs)]
                    (if eneg (- e) e))
                  0)
        scaled (cond
                 (zero? exp-val) base
                 (pos? exp-val) (* base (pow10 exp-val))
                 :else (/ base (pow10 (- exp-val))))]
    (if neg (- scaled) scaled)))

(defn format-2f
  "Two decimals, halves away from zero, like the old print did."
  [v]
  (let [neg (neg? v)
        a (if neg (- v) v)
        scaled (long (+ (* a 100.0) 0.5))
        dollars (quot scaled 100)
        cents (rem scaled 100)]
    (str (when neg "-")
         dollars
         "."
         (when (< cents 10) "0")
         cents)))

(defn build-tableau
  "Flat tableau vector plus col counts for the solve loop."
  [m n c rows rhs]
  (let [cols (+ n m 1)
        rhs-col (+ n m)
        t (transient (vec (repeat (* (inc m) cols) 0.0)))
        t (loop [j 0
                 t t]
            (if (>= j n)
              t
              (recur (inc j) (assoc! t (+ (* m cols) j) (- (nth c j))))))
        t (loop [i 0
                 t t]
            (if (>= i m)
              t
              (let [t (loop [j 0
                             t t]
                        (if (>= j n)
                          t
                          (recur (inc j)
                                 (assoc! t
                                         (+ (* i cols) j)
                                         (nth (nth rows i) j)))))]
                (recur (inc i)
                       (-> t
                           (assoc! (+ (* i cols) (+ n i)) 1.0)
                           (assoc! (+ (* i cols) rhs-col) (nth rhs i)))))))]
    {:tab (persistent! t) :cols cols :rhs rhs-col}))

(defn entering-col
  "Most negative reduced cost col, or -1 when optimal."
  [tab m cols nm]
  (loop [j 0
         best -1
         best-val (- eps)]
    (if (>= j nm)
      best
      (let [v (nth tab (+ (* m cols) j))]
        (if (< v best-val)
          (recur (inc j) j v)
          (recur (inc j) best best-val))))))

(defn leaving-row
  "Min ratio row for the entering col, or -1 when unbounded."
  [tab m cols enter rhs-col]
  (loop [i 0
         best -1
         best-ratio ratio-inf]
    (if (>= i m)
      best
      (let [coeff (nth tab (+ (* i cols) enter))]
        (if (> coeff eps)
          (let [ratio (/ (nth tab (+ (* i cols) rhs-col)) coeff)]
            (if (< ratio (- best-ratio eps))
              (recur (inc i) i ratio)
              (recur (inc i) best best-ratio)))
          (recur (inc i) best best-ratio))))))

(defn pivot-step
  "Normalize the pivot row, then clear the pivot col elsewhere."
  [tab m cols rhs-col prow pcol]
  (let [pivot (nth tab (+ (* prow cols) pcol))
        tab1 (persistent!
              (loop [j 0
                     t (transient tab)]
                (if (> j rhs-col)
                  t
                  (let [k (+ (* prow cols) j)]
                    (recur (inc j) (assoc! t k (/ (nth tab k) pivot)))))))
        tab2 (persistent!
              (loop [i 0
                     t (transient tab1)]
                (if (> i m)
                  t
                  (if (= i prow)
                    (recur (inc i) t)
                    (let [factor (nth tab1 (+ (* i cols) pcol))]
                      (if (> (abs-num factor) eps)
                        (recur (inc i)
                               (loop [j 0
                                      t t]
                                 (if (> j rhs-col)
                                   t
                                   (let [k (+ (* i cols) j)]
                                     (recur (inc j)
                                            (assoc! t
                                                    k
                                                    (- (nth tab1 k)
                                                       (* factor
                                                          (nth tab1
                                                                (+ (* prow cols)
                                                                   j))))))))))
                        (recur (inc i) t)))))))]
    tab2))

(defn solve-lp
  "Pivot loop. Gives [:optimal value] or [:unbounded nil]."
  [tab m cols rhs-col nm]
  (loop [tab tab]
    (let [enter (entering-col tab m cols nm)]
      (if (== enter -1)
        [:optimal (nth tab (+ (* m cols) rhs-col))]
        (let [leave (leaving-row tab m cols enter rhs-col)]
          (if (== leave -1)
            [:unbounded nil]
            (recur (pivot-step tab m cols rhs-col leave enter))))))))

(defn -main
  "Read stdin, run simplex, print the verdict."
  []
  (let [input (slurp *in*)]
    (when-not (str/blank? input)
      (let [toks (vec (str/split (str/trim input) #"\s+"))]
        (cond
          (< (count toks) 2)
          (fail "Error: need m and n")

          (not (every? int-token? (take 2 toks)))
          (fail "Error: m and n must be integers")

          :else
          (let [m (parse-int (nth toks 0))
                nn (parse-int (nth toks 1))
                need (+' 2 nn (*' m (+' nn 1)))]
            (cond
              (or (neg? m) (neg? nn))
              (fail "Error: m and n must be non-negative")

              (< (count toks) need)
              (fail "Error: input too short for given m and n")

              (not (every? double-token? (take need (drop 2 toks))))
              (fail "Error: costs and rows must be numbers")

              :else
              (let [nums (mapv parse-dec (take need (drop 2 toks)))
                    c (subvec nums 0 nn)
                    body (subvec nums nn)
                    width (+ nn 1)
                    rows (mapv (fn [i]
                                 (subvec body (* i width) (+ (* i width) nn)))
                               (range m))
                    rhs (mapv (fn [i] (nth body (+ (* i width) nn)))
                              (range m))
                    {:keys [tab cols rhs]} (build-tableau m nn c rows rhs)
                    [status value] (solve-lp tab m cols rhs (+ nn m))]
                (if (= status :optimal)
                  (do
                    (println "OPTIMAL")
                    (println (format-2f value)))
                  (println "UNBOUNDED"))))))))))

(-main)
