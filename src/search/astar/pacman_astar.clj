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

(defn astar-path
  "Run the A star relaxation over grid rows n and cols m, returning the path
   from start to food as a vector of cell ids, else nil when unreachable."
  [grid n m pr pc fr fc]
  (let [nm (* n m)
        start (+ (* pr m) pc)
        goal (+ (* fr m) fc)
        h (fn [r c] (food-dist r c fr fc))
        nbrs [[-1 0] [0 -1] [0 1] [1 0]]]
    (loop [dist (assoc (vec (repeat nm -1)) start 0)
           par (vec (repeat nm -1))
           costq (assoc (vec (repeat nm 0)) start (h pr pc))
           open (sorted-set [(h pr pc) pr pc])]
      (if (empty? open)
        (when (not= (nth dist goal) -1)
          (loop [s goal acc (list)]
            (if (= s start)
              (vec (conj acc s))
              (recur (nth par s) (conj acc s)))))
        (let [cur (first open)
              [cost cr ccol] cur
              ci (+ (* cr m) ccol)
              dc (nth dist ci)
              [dist par costq open]
              (loop [ks nbrs
                     dist dist
                     par par
                     costq costq
                     open (disj open cur)]
                (if (empty? ks)
                  [dist par costq open]
                  (let [[dr dcol] (first ks)
                        nr (+ cr dr)
                        nc (+ ccol dcol)]
                    (if (and (<= 0 nr) (< nr n) (<= 0 nc) (< nc m)
                             (not= (nth grid (+ (* nr m) nc)) \%))
                      (let [ni (+ (* nr m) nc)
                            nd (nth dist ni)
                            new-dist (inc dc)
                            new-cost (+ dc (h nr nc))]
                        (if (or (= nd -1) (> nd new-dist))
                          (let [open (if (= nd -1)
                                       open
                                       (disj open [(nth costq ni) nr nc]))]
                            (recur (rest ks)
                                   (assoc dist ni new-dist)
                                   (assoc par ni ci)
                                   (assoc costq ni new-cost)
                                   (conj open [new-cost nr nc])))
                          (recur (rest ks) dist par costq open)))
                      (recur (rest ks) dist par costq open)))))]
          (recur dist par costq open))))))

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
