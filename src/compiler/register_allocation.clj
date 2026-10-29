(ns compiler.register-allocation
  "Chaitin graph-coloring register allocation (simplify/spill/select).

   Time: O((n + m) log n) via degree buckets: each edge triggers O(1)
   degree updates and each node enters/leaves sorted worklists in O(log n).
   HEAD rescanned all remaining nodes and their adjacency per step in
   O(n * (n + m)). Coloring is O(n * k + m). Space: O(n + m).
   The simplify and spill choices (smallest id among eligible) and the
   coloring order match the original exactly."
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

(defn -main
  [& _args]
  (let [s (slurp *in*)]
    (when-not (str/blank? s)
      (let [toks (vec (remove str/blank? (str/split s #"\s+")))
            n (parse-long (get toks 0))
            m (parse-long (get toks 1))
            k (parse-long (get toks 2))]
        (cond
          (or (nil? n) (nil? m) (nil? k) (neg? n) (neg? m) (neg? k))
          (error "invalid input: expected n m k header")

          (< (count toks) (+' 3 (*' m 2)))
          (error "invalid input: truncated edge list")

          :else
          (let [pairs (take (* m 2) (drop 3 toks))
                edges (vec (for [[a b] (partition 2 pairs)]
                             [(parse-long a) (parse-long b)]))]
            (if (or (some (fn [[u v]] (or (nil? u) (nil? v))) edges)
                    (some (fn [[u v]]
                            (or (< u 0) (>= u n) (< v 0) (>= v n)))
                          edges))
              (error "invalid input: bad edge")
              (let [adj (reduce (fn [acc [u v]]
                                  (-> acc
                                      (update u conj v)
                                      (update v conj u)))
                                (vec (repeat n []))
                                edges)
                    degrees (vec (map count adj))
                    active (into (sorted-set) (range n))
                    low (into (sorted-set)
                              (filter #(< (nth degrees %) k) (range n)))
                    [select-stack spilled]
                    (loop [active active
                           low low
                           degrees degrees
                           select-stack ()
                           spilled #{}]
                      (if (empty? active)
                        [select-stack spilled]
                        (let [node (or (first low) (first active))
                              simplify? (contains? low node)
                              active2 (disj active node)
                              low2 (disj low node)
                              freqs (frequencies (nth adj node))
                              [degrees2 low3]
                              (reduce (fn [[degs lw] [v cnt]]
                                        (if (contains? active2 v)
                                          (let [old (nth degs v)
                                                new (- old cnt)
                                                degs (assoc degs v new)]
                                            (if (and (>= old k) (< new k))
                                              [degs (conj lw v)]
                                              [degs lw]))
                                          [degs lw]))
                                      [degrees low2]
                                      freqs)]
                          (if simplify?
                            (recur active2 low3 degrees2
                                   (conj select-stack node) spilled)
                            (recur active2 low3 degrees2
                                   select-stack (conj spilled node))))))
                    [color spilled-final]
                    (loop [stk select-stack
                           color (vec (repeat n -1))
                           spilled spilled]
                      (if (empty? stk)
                        [color spilled]
                        (let [u (first stk)
                              used (into #{}
                                         (keep (fn [v]
                                                 (let [c (nth color v)]
                                                   (when (not= c -1) c)))
                                               (nth adj u)))
                              c (first (filter #(not (contains? used %))
                                               (range k)))]
                          (if c
                            (recur (rest stk) (assoc color u c) spilled)
                            (recur (rest stk) color (conj spilled u))))))]
                (doseq [i (range n)]
                  (if (contains? spilled-final i)
                    (println (str "v" i ":SPILL"))
                    (println (str "v" i ":R" (nth color i)))))))))))))

(-main)
