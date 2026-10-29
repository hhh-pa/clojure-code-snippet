;; Ring leader election message counts for the LCR and HS protocols.
;; Reads n followed by n ids, then prints the leader and both message totals.
;; Each LCR message run stops at the next greater id, and one circular
;; monotonic stack finds every such distance in O(n) total time.
;; Each HS probe also stops at the next greater id in its direction, so with
;; both directional distance tables the HS total is a closed form over O(log n)
;; phases: O(n log n) time and O(n) space overall.
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

(defn next-greater-dists
  "Circular distance from each index to the next strictly greater element,
   or n when none exists. Runs in O(n) with a monotonic stack."
  [ring]
  (let [n (count ring)
        v (vec ring)]
    (loop [j (dec (* 2 n)), stack [], d (vec (repeat n n))]
      (if (neg? j)
        d
        (let [val (nth v (rem j n))
              kept (loop [s stack]
                     (if (and (seq s) (<= (nth v (rem (peek s) n)) val))
                       (recur (pop s))
                       s))]
          (recur (dec j)
                 (conj kept j)
                 (if (< j n)
                   (if (seq kept) (assoc d j (- (peek kept) j)) d)
                   d)))))))

(defn prev-greater-dists
  "Circular distance from each index to the previous strictly greater element,
   or n when none exists. Runs in O(n) via the reversed ring."
  [ring]
  (let [n (count ring)
        fwd (next-greater-dists (vec (reverse (vec ring))))]
    (vec (map (fn [i] (nth fwd (- (dec n) i))) (range n)))))

(defn lcr-messages
  "Total LCR messages: each node sends up to its next greater id, or all the
   way around when it holds a maximal id."
  [dists]
  (reduce + 0 dists))

(defn phase-limits
  "HS probe limits 1, 2, 4, and so on up to half the ring."
  [n]
  (loop [lim 1, acc []]
    (if (>= lim (quot n 2))
      (conj acc lim)
      (recur (* lim 2) (conj acc lim)))))

(defn hs-messages
  "Total HS messages from both directional distance tables. A probe of limit
   L sends d messages when the next greater id is d steps away within the
   limit, else L probes plus L replies."
  [d-next d-prev n]
  (if (<= n 1)
    (if (zero? n) 0 1)
    (reduce + 0
            (for [lim (phase-limits n)
                  d (concat d-next d-prev)]
              (if (<= d lim) d (* 2 lim))))))

(defn ring-leader
  "Leader id: the maximum ring id, or -1 when the ring is empty."
  [ids]
  (reduce (fn [m v] (if (> v m) v m)) -1 ids))

(defn simulate
  "Build output lines from toks, or fail to stderr and return nil."
  [toks]
  (if (empty? toks)
    (fail! "Invalid input: expected n followed by n ids")
    (let [n (parse-int (first toks))]
      (cond
        (or (nil? n) (neg? n))
        (fail! "Invalid input: n must be a non-negative integer")
        (< (count toks) (inc' n))
        (fail! "Invalid input: fewer ids than n claims")
        :else
        (let [ids (mapv parse-int (take n (rest toks)))]
          (if (some nil? ids)
            (fail! "Invalid input: ids must be integers")
            (let [d-next (next-greater-dists ids)
                  d-prev (prev-greater-dists ids)]
              [(str "LEADER: " (ring-leader ids))
               (str "LCR MESSAGES: " (lcr-messages d-next))
               (str "HS MESSAGES: " (hs-messages d-next d-prev (count ids)))])))))))

(defn -main
  "Entry point: tokenize INPUT and print result lines. Blank input prints
   nothing. Invalid input prints a note to stderr with no stdout."
  [input]
  (when-not (str/blank? input)
    (let [lines (simulate (tokenize input))]
      (when (some? lines)
        (run! println lines)))))

(-main (slurp *in*))
