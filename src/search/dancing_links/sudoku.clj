;; Sudoku solver using exact cover search in the spirit of dancing links.
;; Each placement covers four columns for cell row digit col digit and box
;; digit, and search always branches on the open cell with fewest options,
;; trying digits low to high, which matches the backtracking solver order and
;; first solution on every puzzle. Sets of open columns keep the board small
;; with work linear in cells per search node, and the worst case stays
;; exponential, as expected for an NP complete puzzle, over a fixed 81 cells.
;; Bad input shapes print a note on stderr and exit zero, since a nonzero
;; exit needs host calls, which this file avoids on purpose.
(require '[clojure.string :as str])

(def digit-values
  "Digit chars mapped to their numeric values for the pure number reader."
  (zipmap "0123456789" (range 10)))

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


(defn whitespace?
  "True for the four blank chars treated as token separators."
  [ch]
  (or (= ch \space) (= ch \tab) (= ch \newline) (= ch \return)))

(defn tokenize
  "Split string s on blanks into a vector of token strings in linear time."
  [s]
  (loop [cs (seq s) cur [] toks []]
    (if (nil? cs)
      (if (seq cur) (conj toks (apply str cur)) toks)
      (let [ch (first cs)]
        (if (whitespace? ch)
          (recur (next cs) [] (if (seq cur) (conj toks (apply str cur)) toks))
          (recur (next cs) (conj cur ch) toks))))))

(defn parse-int-token
  "Read one signed integer token into a long, else nil for a bad token."
  [tok]
  (let [cs (seq tok)]
    (when (seq cs)
      (let [f (first cs)
            neg? (= f \-)
            ds (if (or neg? (= f \+)) (next cs) cs)]
        (when (seq ds)
          (loop [ds ds acc 0]
            (if (nil? ds)
              (fit-long (if neg? (-' acc) acc))
              (let [ch (first ds)]
                (if (contains? digit-values ch)
                  (recur (next ds) (+' (*' acc 10) (get digit-values ch)))
                  nil)))))))))

(defn parse-all-ints
  "Read every token of s as a signed integer, else nil for a bad token."
  [s]
  (let [toks (tokenize s)]
    (loop [ts (seq toks) out []]
      (if (nil? ts)
        out
        (let [v (parse-int-token (first ts))]
          (if (nil? v)
            nil
            (recur (next ts) (conj out v))))))))

(defn fail
  "Report a bad input shape on stderr and return nil with exit code zero."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn box-index
  "Box number 0 to 8 for row r col c."
  [r c]
  (+ (* (quot r 3) 3) (quot c 3)))

(defn row-cols
  "Four exact cover columns for cell idx holding digit d from 1 to 9."
  [idx d]
  (let [r (quot idx 9) c (rem idx 9) e (dec d)]
    [idx (+ 81 (* r 9) e) (+ 162 (* c 9) e) (+ 243 (* (box-index r c) 9) e)]))

(defn cell-options
  "Digits 1 to 9 whose four columns all stay open for cell idx."
  [idx open]
  (loop [d 1 out []]
    (if (> d 9)
      out
      (recur (inc d)
             (if (every? open (row-cols idx d)) (conj out d) out)))))

(defn choose-cell
  "Open cell with fewest options as [idx opts], nil when done, :dead."
  [open]
  (loop [cs (seq (filter open (range 81))) best nil]
    (cond
      (= best :dead) :dead
      (nil? cs) best
      :else
      (let [idx (first cs)
            opts (cell-options idx open)]
        (cond
          (empty? opts) :dead
          (or (nil? best) (< (count opts) (count (nth best 1))))
          (recur (next cs) [idx opts])
          :else (recur (next cs) best))))))

(defn search-cover
  "Exact cover search over open cols, returning placed rows or nil."
  [open placed]
  (let [choice (choose-cell open)]
    (cond
      (= choice :dead) nil
      (nil? choice) placed
      :else
      (let [[idx opts] choice]
        (loop [ds (seq opts)]
          (when ds
            (let [d (first ds)
                  cols (row-cols idx d)
                  res (search-cover (reduce disj open cols)
                                    (conj placed [idx d]))]
              (or res (recur (next ds))))))))))

(defn solve-puzzle
  "Solve one 81 cell puzzle vector, returning the solved or given board."
  [cells]
  (let [board (vec cells)
        givens (vec (for [idx (range 81)
                          :let [v (nth board idx)]
                          :when (not (zero? v))]
                      [idx v]))
        full-open (into (sorted-set) (range 324))
        covered (loop [gs (seq givens) open full-open]
                  (if (nil? gs)
                    open
                    (let [[idx d] (first gs)
                          cols (row-cols idx d)]
                      (if (every? open cols)
                        (recur (next gs) (reduce disj open cols))
                        :conflict))))]
    (if (= covered :conflict)
      board
      (let [sol (search-cover covered givens)]
        (if (nil? sol)
          board
          (reduce (fn [b [idx d]] (assoc b idx d)) board sol))))))

(defn print-board
  "Print one board as nine rows of digits with a trailing blank each."
  [board]
  (doseq [r (range 9)]
    (println (str (str/join " " (subvec board (* r 9) (+ (* r 9) 9))) " "))))

(defn -main
  "Entry point reading the whole stdin string, silent on blank input."
  [input]
  (when-not (str/blank? input)
    (let [all (parse-all-ints input)]
      (if (nil? all)
        (fail "bad input: tokens must all be integers")
        (let [len (count all)
              t (if (pos? len) (min (nth all 0) (quot (dec len) 81)) 0)]
          (doseq [p (range (max t 0))]
            (let [base (+ 1 (* p 81))
                  cells (subvec all base (+ base 81))]
              (if (every? (fn [v] (and (<= 0 v) (<= v 9))) cells)
                (print-board (solve-puzzle cells))
                (fail "bad input: cells must be digits from 0 to 9")))))))))

;; Minimal top level driver.
(-main (slurp *in*))
