(ns compiler.dfa-minimization
  "DFA minimization via Hopcroft's partition-refinement algorithm.

   Time: O(|Sigma| * n log n) expected: each refinement step processes only
   blocks intersecting the predecessor set (via a state->block index), and
   the smaller-half worklist rule moves each state O(log n) times. HEAD
   scanned every block per work item (O(n) scan) and the whole worklist
   per split. Space: O(n * |Sigma|) transitions plus O(n) partitions.
   Hopcroft's bound is asymptotically optimal for DFA minimization."
  (:refer-clojure :exclude [parse-long])
  (:require [clojure.set :as set]
            [clojure.string :as str]))

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

(defn reachable-set
  "BFS from `start` over `trans` (vector of {char -> state})."
  [trans start]
  (loop [seen #{start}
         q (queue-from [start])]
    (if (q-empty? q)
      seen
      (let [u (q-peek q)
            q (q-pop q)]
        (let [[seen q]
              (reduce (fn [[sn qq] v]
                        (if (contains? sn v)
                          [sn qq]
                          [(conj sn v) (q-conj qq v)]))
                      [seen q]
                      (vals (nth trans u)))]
          (recur seen q))))))

(defn predecessors
  "Reachable predecessors of `a-set` on symbol `c`."
  [a-set c rev reachable]
  (reduce (fn [acc a]
            (reduce (fn [acc2 u]
                      (if (contains? reachable u)
                        (conj acc2 u)
                        acc2))
                    acc
                    (get-in rev [a c] [])))
          (sorted-set)
          a-set))

(defn hopcroft
  "Minimize: returns [blocks state->bid] where blocks is {bid -> sorted-set}."
  [trans rev reachable accept-states alphabet]
  (let [states (filter reachable (range (count trans)))
        F (into (sorted-set) (filter accept-states states))
        nonF (into (sorted-set) (remove accept-states states))
        inits (cond-> []
                (seq F) (conj F)
                (seq nonF) (conj nonF))]
    (if (empty? inits)
      [{} {}]
      (let [blocks (into {} (map-indexed vector inits))
            state->bid (reduce (fn [m [bid part]]
                                 (reduce #(assoc %1 %2 bid) m part))
                               {} blocks)
            next-bid (count blocks)
            init-bid (if (and (seq F) (seq nonF))
                       (if (<= (count F) (count nonF)) 0 1)
                       0)
            Wq-init (queue-from [init-bid])
            Win-init #{init-bid}]
        (loop [blocks blocks
               state->bid state->bid
               next-bid next-bid
               Wq Wq-init
               Win Win-init]
          (if (q-empty? Wq)
            [blocks state->bid]
            (let [a-bid (q-peek Wq)
                  Wq (q-pop Wq)
                  Win (disj Win a-bid)
                  A (get blocks a-bid (sorted-set))
                  [blocks state->bid next-bid Wq Win]
                  (reduce
                   (fn [[blks s2b nbid wq win] c]
                     (let [X (predecessors A c rev reachable)]
                       (if (empty? X)
                         [blks s2b nbid wq win]
                         (let [by-block (reduce (fn [m u]
                                                  (update m (get s2b u)
                                                          (fnil conj (sorted-set)) u))
                                                {} X)
                               affected (filter (fn [[bid inter]]
                                                  (let [Y (get blks bid)]
                                                    (and Y (< (count inter) (count Y)))))
                                                by-block)]
                           (reduce
                            (fn [[blks s2b nbid wq win] [bid inter]]
                              (let [Y (get blks bid)
                                    diff (set/difference Y inter)
                                    new-bid nbid
                                    blks (-> blks
                                             (assoc bid inter)
                                             (assoc new-bid diff))
                                    s2b (reduce #(assoc %1 %2 new-bid) s2b diff)
                                    in-w? (contains? win bid)]
                                (if in-w?
                                  [blks s2b (inc new-bid)
                                   (q-conj wq new-bid) (conj win new-bid)]
                                  (if (<= (count inter) (count diff))
                                    [blks s2b (inc new-bid)
                                     (q-conj wq bid) (conj win bid)]
                                    [blks s2b (inc new-bid)
                                     (q-conj wq new-bid) (conj win new-bid)]))))
                            [blks s2b nbid wq win]
                            affected)))))
                   [blocks state->bid next-bid Wq Win]
                   alphabet)]
              (recur blocks state->bid next-bid Wq Win))))))))

(defn -main
  [& _args]
  (let [s (slurp *in*)]
    (when-not (str/blank? s)
      (let [toks (vec (remove str/blank? (str/split s #"\s+")))
            n (parse-long (get toks 0))
            start-state (parse-long (get toks 1))
            num-accept (parse-long (get toks 2))]
        (cond
          (or (nil? n) (nil? start-state) (nil? num-accept))
          (error "invalid input: expected n start-state num-accept header")

          (or (neg? n) (neg? num-accept) (< start-state 0) (>= start-state n))
          (error "invalid input: bad DFA header values")

          (< (count toks) (+' 3 num-accept 1))
          (error "invalid input: truncated accept list")

          :else
          (let [accept-vals (map parse-long (take num-accept (drop 3 toks)))
                toks2 (vec (drop (+ 3 num-accept) toks))]
            (cond
              (some nil? accept-vals)
              (error "invalid input: bad accept state")

              (some #(or (< % 0) (>= % n)) accept-vals)
              (error "invalid input: accept state out of range")

              :else
              (let [accept-states (set accept-vals)
                    alpha-size (parse-long (get toks2 0))]
                (cond
                  (nil? alpha-size)
                  (error "invalid input: expected alphabet size")

                  (neg? alpha-size)
                  (error "invalid input: negative alphabet size")

                  (< (count toks2) (+' 1 alpha-size 1))
                  (error "invalid input: truncated alphabet")

                  :else
                  (let [alphabet (vec (map first (take alpha-size (rest toks2))))
                        toks3 (vec (drop (+ 1 alpha-size) toks2))
                        num-trans (parse-long (get toks3 0))
                        rest-toks (vec (rest toks3))]
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
                                        (or (< u 0) (>= u n) (< v 0) (>= v n)))
                                      parsed))
                          (error "invalid input: bad transition")
                          (let [trans (reduce (fn [t [u v c]]
                                                (assoc-in t [u (first c)] v))
                                              (vec (repeat n {})) parsed)
                                rev (reduce (fn [r [u v c]]
                                              (update-in r [v (first c)]
                                                         (fnil conj []) u))
                                            (vec (repeat n {})) parsed)
                                reachable (reachable-set trans start-state)
                                [blocks state->bid]
                                (hopcroft trans rev reachable accept-states alphabet)
                                P (vals blocks)
                                min-start (get state->bid start-state)
                                min-accept (into (sorted-set)
                                                 (keep (fn [[bid part]]
                                                         (when (some accept-states part) bid))
                                                       blocks))
                                min-trans (into {}
                                                (for [[bid part] blocks
                                                      :let [rep (first part)]]
                                                  [bid (into {}
                                                             (for [[c v] (nth trans rep)
                                                                   :when (contains? reachable v)]
                                                               [c (get state->bid v)]))]))]
                            (println (str "Minimal DFA states: " (count P)))
                            (println (str "Accept states: " (count min-accept)))
                            (when (seq tail)
                              (let [num-queries (parse-long (first tail))]
                                (if (nil? num-queries)
                                  (error "invalid input: expected query count")
                                  (let [queries (take num-queries (rest tail))]
                                    (if (< (count (rest tail)) num-queries)
                                      (error "invalid input: truncated query list")
                                      (doseq [w queries]
                                        (let [w (if (= w "EPS") "" w)]
                                          (loop [curr min-start
                                                 chars (seq w)]
                                            (if (nil? chars)
                                              (if (contains? min-accept curr)
                                                (println "ACCEPT")
                                                (println "REJECT"))
                                              (let [ch (first chars)
                                                    nxt (get (get min-trans curr) ch)]
                                                (if (nil? nxt)
                                                  (println "REJECT")
                                                  (recur nxt (next chars)))))))))))))))))))))))))))

(-main)
