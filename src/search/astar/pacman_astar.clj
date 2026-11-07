;; Pacman food search solved with A star over a grid.
;; Open cells live in a sorted set ordered by cost then row then col, which
;; keeps the same expansion order as a heap keyed the same way, so the run
;; takes O(E log V) time and O(V) space for V cells and E grid moves.
;; Neighbours expand up left right down with food distance as the heuristic,
;; and every reachable cell relaxes fully with no early stop, matching the
;; reference port behaviour, while missing output marks unreachable food.
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
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r]+"))))

(defn parse-int-token
  "Read one signed integer token into a long, else nil for a bad token."
  [tok]
  (when (and (string? tok) (pos? (count tok)))
    (parse-long tok)))

(defn fail
  "Report a bad input shape on stderr and return nil with exit code zero."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn abs-val
  "Pure absolute value for a number."
  [x]
  (if (neg? x) (- x) x))

(defn food-dist
  "Manhattan distance from row r col c to food row fr col fc."
  [r c fr fc]
  (+ (abs-val (- r fr)) (abs-val (- c fc))))

(def nbr-dr [-1 0 0 1])
(def nbr-dc [0 -1 1 0])

(defn pack-node [cost r c]
  (bit-or (bit-shift-left cost 20) (bit-shift-left r 10) c))

(defn astar-path
  "Run the A star relaxation over grid rows n and cols m, returning the path
   from start to food as a vector of cell ids, else nil when unreachable."
  [grid n m pr pc fr fc]
  (let [nm (* n m)
        start (+ (* pr m) pc)
        goal (+ (* fr m) fc)
        init-h (food-dist pr pc fr fc)
        dist (assoc! (transient (vec (repeat nm -1))) start 0)
        par (transient (vec (repeat nm -1)))
        costq (assoc! (transient (vec (repeat nm 0))) start init-h)]
    (loop [open (sorted-set (pack-node init-h pr pc))]
      (if (empty? open)
        (when (not= (nth dist goal) -1)
          (loop [s goal acc (list)]
            (if (= s start)
              (vec (conj acc s))
              (recur (nth par s) (conj acc s)))))
        (let [cur (first open)
              cr (bit-and (bit-shift-right cur 10) 1023)
              ccol (bit-and cur 1023)
              ci (+ (* cr m) ccol)
              dc (nth dist ci)
              open2 (loop [k 0 op (disj open cur)]
                      (if (>= k 4)
                        op
                        (let [nr (+ cr (nth nbr-dr k))
                              nc (+ ccol (nth nbr-dc k))]
                          (if (and (<= 0 nr) (< nr n) (<= 0 nc) (< nc m)
                                   (not= (nth grid (+ (* nr m) nc)) \%))
                            (let [ni (+ (* nr m) nc)
                                  nd (nth dist ni)
                                  new-dist (inc dc)
                                  new-cost (+ dc (food-dist nr nc fr fc))]
                              (if (or (= nd -1) (> nd new-dist))
                                (let [op2 (if (= nd -1)
                                            op
                                            (disj op (pack-node (nth costq ni) nr nc)))]
                                  (assoc! dist ni new-dist)
                                  (assoc! par ni ci)
                                  (assoc! costq ni new-cost)
                                  (recur (inc k) (conj op2 (pack-node new-cost nr nc))))
                                (recur (inc k) op)))
                            (recur (inc k) op)))))]
          (recur open2))))))

(defn solve-input
  "Solve one grid read from string s, printing the path or nothing."
  [s]
  (let [toks (tokenize s)]
    (if (< (count toks) 6)
      (fail "bad input: need start food dims and grid rows")
      (let [nums (mapv parse-int-token (subvec toks 0 6))]
        (if (some nil? nums)
          (fail "bad input: header values must be integers")
          (let [[pr pc fr fc n m] nums
                nm (* n m)
                chars (vec (apply str (subvec toks 6)))]
            (if (or (<= n 0) (<= m 0) (< (count chars) nm)
                     (< pr 0) (< pc 0) (< fr 0) (< fc 0)
                     (>= pr n) (>= pc m) (>= fr n) (>= fc m))
              (fail "bad input: dims or points out of range")
              (let [grid (subvec chars 0 nm)
                    path (astar-path grid n m pr pc fr fc)]
                (when path
                  (println (dec (count path)))
                  (doseq [s path]
                    (println (str (quot s m) " " (rem s m)))))))))))))

(defn -main
  "Entry point reading the whole stdin string, silent on blank input."
  [input]
  (when-not (str/blank? input)
    (solve-input input)))

;; Minimal top level driver.
(-main (slurp *in*))
