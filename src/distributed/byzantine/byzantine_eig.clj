;; Simplified exponential information gather: loyal decision broadcast.
;; Reads n f commander-value num-faulty, then that many faulty ids.
;; Prints one DECISION line plus one line per loyal non-commander process.
;; Time is O(n) and space is O(num-faulty), which is optimal here because
;; every loyal process needs its own output line. Blank input prints nothing.
;; Invalid input prints a note to stderr and returns normally with no stdout.
;; A nonzero exit would need host interop, which is retired, so exit stays 0.

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

(defn eig-decision
  "Loyal decision: 0 when the commander is faulty, else the sent value."
  [commander-val faulty]
  (if (contains? faulty 0) 0 commander-val))

(defn decision-lines
  "One DECISION line plus one line per loyal non-commander process."
  [n decision faulty]
  (into [(str "DECISION: " decision)]
        (comp (filter #(not (contains? faulty %)))
              (map #(str "P" % ": " decision)))
        (range 1 n)))

(defn simulate
  "Build output lines from toks, or fail to stderr and return nil."
  [toks]
  (if (< (count toks) 4)
    (fail! "Invalid input: expected n f commander-value num-faulty ...")
    (let [n (parse-int (nth toks 0))
          f (parse-int (nth toks 1))
          commander-val (parse-int (nth toks 2))
          num-faulty (parse-int (nth toks 3))]
      (cond
        (or (nil? n) (neg? n))
        (fail! "Invalid input: n must be a non-negative integer")
        (nil? f)
        (fail! "Invalid input: f must be an integer")
        (nil? commander-val)
        (fail! "Invalid input: commander value must be an integer")
        (or (nil? num-faulty) (neg? num-faulty))
        (fail! "Invalid input: num-faulty must be a non-negative integer")
        (< (count toks) (+' 4 num-faulty))
        (fail! "Invalid input: fewer faulty ids than num-faulty claims")
        :else
        (let [ids (mapv parse-int (take num-faulty (drop 4 toks)))]
          (if (some nil? ids)
            (fail! "Invalid input: faulty ids must be integers")
            (let [faulty (set ids)]
              (decision-lines n (eig-decision commander-val faulty) faulty))))))))

(defn -main
  "Entry point: tokenize INPUT and print result lines. Blank input prints
   nothing. Invalid input prints a note to stderr with no stdout."
  [input]
  (when-not (str/blank? input)
    (let [lines (simulate (tokenize input))]
      (when (some? lines)
        (run! println lines)))))

(-main (slurp *in*))
