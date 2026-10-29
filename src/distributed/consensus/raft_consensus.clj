;; Raft leader election and log replication sketch.
;; Reads num-nodes num-events, then START_ELECTION c / APPEND_ENTRY l cmd /
;; PRINT_STATE p events.
;; Each event visits every node once: O(events N) time and O(N) space, which is
;; optimal for a faithful full-state simulation. Blank input prints nothing.
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

(defn valid-node?
  "True when p names one of n nodes."
  [n p]
  (and (some? p) (integer? p) (<= 0 p) (< p n)))

(defn fresh-state
  "Fresh cluster state for n nodes."
  [n]
  {:roles (vec (repeat n "FOLLOWER"))
   :terms (vec (repeat n 0))
   :voted (vec (repeat n -1))
   :commits (vec (repeat n -1))
   :logsz 0})

(defn start-election
  "Run START_ELECTION for candidate c. Returns [next-state line]."
  [{:keys [roles terms voted] :as state} c majority]
  (let [t (inc (nth terms c))
        n (count roles)
        [roles2 terms2 voted2 votes]
        (reduce (fn [[rl tm vd votes] i]
                  (if (= i c)
                    [rl tm vd votes]
                    (if (< (nth tm i) t)
                      [(assoc rl i "FOLLOWER") (assoc tm i t) (assoc vd i c) (inc votes)]
                      [rl tm vd votes])))
                [(assoc roles c "CANDIDATE") (assoc terms c t) (assoc voted c c) 1]
                (range n))]
    (if (>= votes majority)
      [(assoc state :roles (assoc roles2 c "LEADER") :terms terms2 :voted voted2)
       (str "LEADER ELECTED: " c " TERM: " t)]
      [(assoc state :roles (assoc roles2 c "FOLLOWER") :terms terms2 :voted voted2)
       (str "ELECTION FAILED: " c)])))

(defn append-entry
  "Run APPEND_ENTRY from leader l with command cmd. Returns [next-state line]."
  [{:keys [roles terms commits logsz] :as state} l cmd majority]
  (if (not= (nth roles l) "LEADER")
    [state "REJECT NOT LEADER"]
    (let [t (nth terms l)
          n (count roles)
          sz (inc logsz)
          demote (fn [coll v] (reduce (fn [c i] (if (= i l) c (assoc c i v)))
                                       coll
                                       (range n)))
          state2 (assoc state
                        :roles (demote roles "FOLLOWER")
                        :terms (demote terms t)
                        :logsz sz)]
      (if (>= n majority)
        [(assoc state2 :commits (vec (repeat n (dec sz))))
         (str "COMMITTED " cmd " AT INDEX " (dec sz))]
        [state2 nil]))))

(defn state-line
  "One PRINT_STATE line for node p."
  [{:keys [roles terms commits]} p]
  (str "NODE " p " ROLE " (nth roles p) " TERM " (nth terms p)
       " COMMITTED " (nth commits p)))

(defn simulate
  "Build output lines from toks, or fail to stderr and return nil."
  [toks]
  (if (< (count toks) 2)
    (fail! "Invalid input: expected num-nodes num-events ...")
    (let [n (parse-int (nth toks 0))
          n-events (parse-int (nth toks 1))]
      (cond
        (or (nil? n) (neg? n))
        (fail! "Invalid input: num-nodes must be a non-negative integer")
        (or (nil? n-events) (neg? n-events))
        (fail! "Invalid input: num-events must be a non-negative integer")
        :else
        (let [majority (inc (quot n 2))]
          (loop [e 0, ti 2, state (fresh-state n), lines []]
            (if (>= e n-events)
              lines
              (if (>= ti (count toks))
                (fail! "Invalid input: fewer event tokens than num-events claims")
                (let [ev (nth toks ti)]
                  (cond
                    (= ev "START_ELECTION")
                    (if (>= (+ ti 1) (count toks))
                      (fail! "Invalid input: START_ELECTION needs a candidate")
                      (let [c (parse-int (nth toks (+ ti 1)))]
                        (if-not (valid-node? n c)
                          (fail! "Invalid input: candidate out of range")
                          (let [[state2 line] (start-election state c majority)]
                            (recur (inc e) (+ ti 2) state2 (conj lines line))))))
                    (= ev "APPEND_ENTRY")
                    (if (>= (+ ti 2) (count toks))
                      (fail! "Invalid input: APPEND_ENTRY needs a leader and a command")
                      (let [l (parse-int (nth toks (+ ti 1)))]
                        (if-not (valid-node? n l)
                          (fail! "Invalid input: leader out of range")
                          (let [[state2 line] (append-entry state l (nth toks (+ ti 2)) majority)]
                            (recur (inc e) (+ ti 3) state2
                                   (if (some? line) (conj lines line) lines))))))
                    (= ev "PRINT_STATE")
                    (if (>= (+ ti 1) (count toks))
                      (fail! "Invalid input: PRINT_STATE needs a node")
                      (let [p (parse-int (nth toks (+ ti 1)))]
                        (if-not (valid-node? n p)
                          (fail! "Invalid input: node out of range")
                          (recur (inc e) (+ ti 2) state
                                 (conj lines (state-line state p))))))
                    :else
                    (fail! (str "Invalid input: unknown event " ev))))))))))))

(defn -main
  "Entry point: tokenize INPUT and print result lines. Blank input prints
   nothing. Invalid input prints a note to stderr with no stdout."
  [input]
  (when-not (str/blank? input)
    (let [lines (simulate (tokenize input))]
      (when (some? lines)
        (run! println lines)))))

(-main (slurp *in*))
