;; Single-decree Paxos acceptor simulation.
;; Reads num-acceptors num-ops, then PREPARE n / PROPOSE n value ops.
;; Each op scans every acceptor once: O(ops A) time and O(A) space, which is
;; optimal for a faithful per-acceptor simulation. Blank input prints nothing.
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

(defn fresh-state
  "Fresh acceptor state for a acceptors."
  [a]
  {:promised (vec (repeat a 0))
   :accepted (vec (repeat a 0))
   :aval (vec (repeat a ""))})

(defn prepare
  "Run PREPARE(n): promise to higher proposals, report the top accepted value.
   Returns [next-state line]."
  [{:keys [promised accepted aval] :as state} n majority]
  (let [[promised2 cnt top-n top-v]
        (reduce (fn [[pr cnt top-n top-v] i]
                  (if (< (nth pr i) n)
                    (let [acc (nth accepted i)]
                      (if (> acc top-n)
                        [(assoc pr i n) (inc cnt) acc (nth aval i)]
                        [(assoc pr i n) (inc cnt) top-n top-v]))
                    [pr cnt top-n top-v]))
                [promised 0 0 ""]
                (range (count promised)))]
    [(assoc state :promised promised2)
     (if (>= cnt majority)
       (if (pos? top-n)
         (str "PROMISED " n " PREV " top-n " " top-v)
         (str "PROMISED " n " NONE"))
       (str "PREPARE REJECTED " n))]))

(defn propose
  "Run PROPOSE(n, v): accept at acceptors that promised at most n.
   Returns [next-state line]."
  [{:keys [promised accepted aval] :as state} n v majority]
  (let [[promised2 accepted2 aval2 cnt]
        (reduce (fn [[pr ac av cnt] i]
                  (if (<= (nth pr i) n)
                    [(assoc pr i n) (assoc ac i n) (assoc av i v) (inc cnt)]
                    [pr ac av cnt]))
                [promised accepted aval 0]
                (range (count promised)))]
    [(assoc state :promised promised2 :accepted accepted2 :aval aval2)
     (if (>= cnt majority)
       (str "CHOSEN " n " " v)
       (str "PROPOSE REJECTED " n))]))

(defn simulate
  "Build output lines from toks, or fail to stderr and return nil."
  [toks]
  (if (< (count toks) 2)
    (fail! "Invalid input: expected num-acceptors num-ops ...")
    (let [a (parse-int (nth toks 0))
          n-ops (parse-int (nth toks 1))]
      (cond
        (or (nil? a) (neg? a))
        (fail! "Invalid input: num-acceptors must be a non-negative integer")
        (or (nil? n-ops) (neg? n-ops))
        (fail! "Invalid input: num-ops must be a non-negative integer")
        :else
        (let [majority (inc (quot a 2))]
          (loop [i 0, ti 2, state (fresh-state a), lines []]
            (if (>= i n-ops)
              lines
              (if (>= ti (count toks))
                (fail! "Invalid input: fewer op tokens than num-ops claims")
                (let [op (nth toks ti)]
                  (cond
                    (= op "PREPARE")
                    (if (>= (+ ti 1) (count toks))
                      (fail! "Invalid input: PREPARE needs a proposal number")
                      (let [n (parse-int (nth toks (+ ti 1)))]
                        (if (nil? n)
                          (fail! "Invalid input: proposal number must be an integer")
                          (let [[state2 line] (prepare state n majority)]
                            (recur (inc i) (+ ti 2) state2 (conj lines line))))))
                    (= op "PROPOSE")
                    (if (>= (+ ti 2) (count toks))
                      (fail! "Invalid input: PROPOSE needs a number and a value")
                      (let [n (parse-int (nth toks (+ ti 1)))]
                        (if (nil? n)
                          (fail! "Invalid input: proposal number must be an integer")
                          (let [[state2 line] (propose state n (nth toks (+ ti 2)) majority)]
                            (recur (inc i) (+ ti 3) state2 (conj lines line))))))
                    :else
                    (fail! (str "Invalid input: unknown op " op))))))))))))

(defn -main
  "Entry point: tokenize INPUT and print result lines. Blank input prints
   nothing. Invalid input prints a note to stderr with no stdout."
  [input]
  (when-not (str/blank? input)
    (let [lines (simulate (tokenize input))]
      (when (some? lines)
        (run! println lines)))))

(-main (slurp *in*))
