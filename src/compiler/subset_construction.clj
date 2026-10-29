(ns compiler.subset-construction
  "NFA (with # epsilon moves) to DFA subset construction plus simulation.

   Time: O(|DFA| * |Sigma| * (n + m)) where each DFA state expands each
   alphabet symbol with a move + epsilon closure linear in the NFA size;
   worklists use persistent queues for O(1) pops (HEAD used vectors with
   front drops). Total is output-sensitive and optimal for powerset
   construction; simulation is O(|w|) per query. Space: O(|DFA| * n)."
  (:refer-clojure :exclude [parse-long])
  (:require [clojure.string :as str]))

(def digit-values
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


(defn parse-long
  [s]
  (when (and (string? s) (seq s))
    (let [cs (seq s)
          [neg? rest-cs] (cond
                           (= (first cs) \-) [true (next cs)]
                           (= (first cs) \+) [false (next cs)]
                           :else [false cs])]
      (when (seq rest-cs)
        (loop [cs rest-cs
               acc 0]
          (if (nil? cs)
            (fit-long (if neg? (-' acc) acc))
            (let [d (get digit-values (first cs))]
              (when-not (nil? d)
                (recur (next cs) (+' (*' acc 10) d))))))))))

(defn error
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn queue-from
  "Pure FIFO queue as [items read-index]; O(1) amortized ops."
  [coll]
  [(vec coll) 0])

(defn q-empty?
  [[v i]]
  (>= i (count v)))

(defn q-peek
  [[v i]]
  (nth v i))

(defn q-pop
  [[v i]]
  (let [j (inc i)]
    (if (and (> j 64) (> j (quot (count v) 2)))
      [(subvec v j) 0]
      [v j])))

(defn q-conj
  [[v i] x]
  [(conj v x) i])

(defn epsilon-closure
  "All NFA states reachable from `states` via # moves (sorted-set)."
  [states nfa-trans]
  (loop [closure (into (sorted-set) states)
         q (queue-from states)]
    (if (q-empty? q)
      closure
      (let [u (q-peek q)
            q (q-pop q)]
        (let [[closure q]
              (reduce (fn [[cl qq] [c to]]
                        (if (and (= c \#) (not (contains? cl to)))
                          [(conj cl to) (q-conj qq to)]
                          [cl qq]))
                      [closure q]
                      (get nfa-trans u []))]
          (recur closure q))))))

(defn move-states
  "Set of NFA states reachable from `states` on symbol `c`."
  [states c nfa-trans]
  (into (sorted-set)
        (for [s states
              [ch to] (get nfa-trans s [])
              :when (= ch c)]
          to)))

(defn -main
  [& _args]
  (let [s (slurp *in*)]
    (when-not (str/blank? s)
      (let [toks (vec (remove str/blank? (str/split s #"\s+")))
            n-states (parse-long (get toks 0))
            start-state (parse-long (get toks 1))
            num-accept (parse-long (get toks 2))]
        (cond
          (or (nil? n-states) (nil? start-state) (nil? num-accept))
          (error "invalid input: expected n-states start-state num-accept header")

          (or (neg? n-states) (neg? num-accept)
              (< start-state 0) (>= start-state n-states))
          (error "invalid input: bad NFA header values")

          (< (count toks) (+' 3 num-accept 1))
          (error "invalid input: truncated accept list")

          :else
          (let [accept-toks (take num-accept (drop 3 toks))
                accepts (map parse-long accept-toks)
                toks2 (vec (drop (+ 3 num-accept) toks))]
            (cond
              (some nil? accepts)
              (error "invalid input: bad accept state")

              (some #(or (< % 0) (>= % n-states)) accepts)
              (error "invalid input: accept state out of range")

              :else
              (let [nfa-accept (set accepts)
                    num-trans (parse-long (get toks2 0))
                    rest-toks (vec (rest toks2))]
                (cond
                  (nil? num-trans)
                  (error "invalid input: expected transition count")

                  (neg? num-trans)
                  (error "invalid input: negative transition count")

                  (< (count rest-toks) (*' 3 num-trans))
                  (error "invalid input: truncated transition list")

                  :else
                  (let [entries (take (* 3 num-trans) rest-toks)
                        tail (vec (drop (* 3 num-trans) rest-toks))
                        parsed (vec (for [[u v c] (partition 3 entries)]
                                      [(parse-long u) (parse-long v) c]))]
                    (if (or (some (fn [[u v _]] (or (nil? u) (nil? v))) parsed)
                            (some (fn [[u v _]]
                                    (or (< u 0) (>= u n-states)
                                        (< v 0) (>= v n-states)))
                                  parsed))
                      (error "invalid input: bad transition")
                      (let [nfa-trans (reduce (fn [m [u v c]]
                                                (update m u (fnil conj []) [(first c) v]))
                                              {} parsed)
                            alphabet (into (sorted-set)
                                           (comp (map #(first (nth % 2)))
                                                 (remove #(= % \#)))
                                           parsed)
                            start-set (epsilon-closure [start-state] nfa-trans)]
                        (loop [work-queue (queue-from [0])
                               dfa-state-map {start-set 0}
                               dfa-states [start-set]
                               dfa-accept [(boolean (some nfa-accept start-set))]
                               dfa-trans {0 {}}]
                          (if (q-empty? work-queue)
                            (do
                              (println (str "DFA states: " (count dfa-states)))
                              (println (str "DFA accept states: "
                                            (count (filter identity dfa-accept))))
                              (when (seq tail)
                                (let [num-queries (parse-long (first tail))]
                                  (if (nil? num-queries)
                                    (error "invalid input: expected query count")
                                    (let [queries (take num-queries (rest tail))]
                                      (if (< (count (rest tail)) num-queries)
                                        (error "invalid input: truncated query list")
                                        (doseq [w queries]
                                          (let [w (if (= w "EPS") "" w)]
                                            (loop [state 0
                                                   chars (seq w)]
                                              (if (nil? chars)
                                                (if (nth dfa-accept state)
                                                  (println "ACCEPT")
                                                  (println "REJECT"))
                                                (let [ch (first chars)
                                                      nxt (get (get dfa-trans state) ch)]
                                                  (if (nil? nxt)
                                                    (println "REJECT")
                                                    (recur nxt (next chars))))))))))))))
                            (let [curr-id (q-peek work-queue)
                                  work-queue (q-pop work-queue)
                                  curr-set (nth dfa-states curr-id)
                                  [wq sm sts acc tm]
                                  (reduce
                                   (fn [[wq s-map states accept-vec trans-map] c]
                                     (let [moved (move-states curr-set c nfa-trans)
                                           nxt (epsilon-closure moved nfa-trans)]
                                       (if (empty? nxt)
                                         [wq s-map states accept-vec trans-map]
                                         (if-let [existing (get s-map nxt)]
                                           [wq s-map states accept-vec
                                            (assoc-in trans-map [curr-id c] existing)]
                                           (let [nid (count states)
                                                 is-acc (boolean (some nfa-accept nxt))]
                                             [(q-conj wq nid)
                                              (assoc s-map nxt nid)
                                              (conj states nxt)
                                              (conj accept-vec is-acc)
                                              (assoc-in trans-map [curr-id c] nid)])))))
                                   [work-queue dfa-state-map dfa-states
                                    dfa-accept dfa-trans]
                                   alphabet)]
                              (recur wq sm sts acc tm))))))))))))))))

(-main)
