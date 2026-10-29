;; Sequence alignment: global plus local alignment with traceback.
;; Input: mode token then two sequence tokens. Mode GLOBAL selects
;; global alignment, anything else selects local alignment.
;; Output: score, then the two aligned strings.
;; Both tables cost O(n m) time and space, which is optimal for time;
;; the full table is kept because the printed alignment follows the
;; classic diagonal first traceback exactly, and linear space
;; variants pick different but equally scored alignments on ties, so
;; shrinking space would change valid stdout. Score only passes below
;; would need just one row.
;; Pure clojure with no host interop.
(require '[clojure.string :as str])

(def MATCH 2)
(def MISMATCH -1)
(def GAP -2)

(defn- blank-char?
  "True for the whitespace chars that split tokens."
  [c]
  (or (= c \space) (= c \tab) (= c \newline) (= c \return) (= c \formfeed)))

(defn- tokenize
  "Splits s on whitespace; returns a vector of token strings."
  [s]
  (loop [cs (seq s) cur [] toks []]
    (if (nil? cs)
      (if (seq cur) (conj toks (apply str cur)) toks)
      (let [c (first cs)]
        (if (blank-char? c)
          (recur (next cs) [] (if (seq cur) (conj toks (apply str cur)) toks))
          (recur (next cs) (conj cur c) toks))))))

(defn- report
  "Prints msg on stderr."
  [msg]
  (binding [*out* *err*] (println msg)))

(defn- sub-score
  "Substitution score for two chars."
  [c1 c2]
  (if (= c1 c2) MATCH MISMATCH))

(defn- global-table
  "Full global alignment table for char vectors s1 and s2."
  [s1 s2]
  (let [n (count s1)
        m (count s2)
        row0 (mapv #(* GAP %) (range (inc m)))]
    (reduce
     (fn [rows i]
       (let [prev (peek rows)
             c1 (nth s1 (dec i))
             row (transient [(+ (nth prev 0) GAP)])]
         (loop [j 1]
           (when (<= j m)
             (let [diag (+ (nth prev (dec j))
                           (sub-score c1 (nth s2 (dec j))))
                   up (+ (nth prev j) GAP)
                   left (+ (nth row (dec j)) GAP)]
               (conj! row (max diag up left))
               (recur (inc j)))))
         (conj rows (persistent! row))))
     [row0]
     (range 1 (inc n)))))

(defn- traceback-global
  "Diagonal first traceback through a global table."
  [table s1 s2]
  (loop [i (count s1) j (count s2) a1 [] a2 []]
    (if (and (zero? i) (zero? j))
      [(apply str (reverse a1)) (apply str (reverse a2))]
      (let [v (nth (nth table i) j)]
        (cond
          (and (pos? i) (pos? j)
               (= v (+ (nth (nth table (dec i)) (dec j))
                       (sub-score (nth s1 (dec i)) (nth s2 (dec j))))))
          (recur (dec i) (dec j)
                 (conj a1 (nth s1 (dec i))) (conj a2 (nth s2 (dec j))))
          (and (pos? i) (= v (+ (nth (nth table (dec i)) j) GAP)))
          (recur (dec i) j (conj a1 (nth s1 (dec i))) (conj a2 \-))
          :else
          (recur i (dec j) (conj a1 \-) (conj a2 (nth s2 (dec j)))))))))

(defn- solve-global
  "Global alignment score plus aligned strings."
  [s1 s2]
  (let [table (global-table s1 s2)
        [a1 a2] (traceback-global table s1 s2)]
    [(nth (nth table (count s1)) (count s2)) a1 a2]))

(defn- local-table
  "Full local alignment table plus best score and its position."
  [s1 s2]
  (let [n (count s1)
        m (count s2)
        row0 (vec (repeat (inc m) 0))]
    (reduce
     (fn [[rows score bi bj] i]
       (let [prev (peek rows)
             c1 (nth s1 (dec i))
             row (transient [0])]
         (loop [j 1 score score bi bi bj bj]
           (if (> j m)
             [(conj rows (persistent! row)) score bi bj]
             (let [diag (+ (nth prev (dec j))
                           (sub-score c1 (nth s2 (dec j))))
                   up (+ (nth prev j) GAP)
                   left (+ (nth row (dec j)) GAP)
                   v (max 0 diag up left)]
               (conj! row v)
               (if (> v score)
                 (recur (inc j) v i j)
                 (recur (inc j) score bi bj)))))))
     [[row0] 0 0 0]
     (range 1 (inc n)))))

(defn- traceback-local
  "Diagonal first traceback from the best local cell."
  [table s1 s2 bi bj]
  (loop [i bi j bj a1 [] a2 []]
    (if (or (zero? i) (zero? j) (not (pos? (nth (nth table i) j))))
      [(apply str (reverse a1)) (apply str (reverse a2))]
      (let [v (nth (nth table i) j)]
        (cond
          (= v (+ (nth (nth table (dec i)) (dec j))
                  (sub-score (nth s1 (dec i)) (nth s2 (dec j)))))
          (recur (dec i) (dec j)
                 (conj a1 (nth s1 (dec i))) (conj a2 (nth s2 (dec j))))
          (= v (+ (nth (nth table (dec i)) j) GAP))
          (recur (dec i) j (conj a1 (nth s1 (dec i))) (conj a2 \-))
          :else
          (recur i (dec j) (conj a1 \-) (conj a2 (nth s2 (dec j)))))))))

(defn- solve-local
  "Local alignment score plus aligned strings."
  [s1 s2]
  (let [[table score bi bj] (local-table s1 s2)
        [a1 a2] (traceback-local table s1 s2 bi bj)]
    [score a1 a2]))

(defn -main
  "Parses data and prints the score plus aligned strings.
  Malformed input prints a message on stderr and returns normally,
  leaving the exit code at zero since nonzero exit needs host calls."
  [data]
  (when-not (str/blank? data)
    (try
      (let [toks (tokenize data)]
        (if (< (count toks) 3)
          (report "error: expected a mode plus two sequence tokens")
          (let [[mode s1 s2] toks
                [score a1 a2] (if (= mode "GLOBAL")
                                (solve-global (vec s1) (vec s2))
                                (solve-local (vec s1) (vec s2)))]
            (println score)
            (println a1)
            (println a2))))
      (catch Exception _ (report "error: invalid input")))))

(let [data (slurp *in*)] (-main data))
