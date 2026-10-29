(ns compiler.ll1-parser
  "LL(1) grammar analysis (FIRST FOLLOW table) plus predictive parsing.

   Time: FIRST FOLLOW via worklists in O(|G| * |T|) total (each set grows
   at most |T|+1 times and each growth reprocesses only dependent
   productions). The original recomputed every production until
   convergence. Table construction O(|P| * |T|), parsing O(n) per query.
   Space: O(|N| * |T| + |table|). Worklist fixpoints are the standard
   optimal approach for iterative dataflow."
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

(defn uppercase?
  [c]
  (and (<= (compare \A c) 0) (<= (compare c \Z) 0)))

(defn terminal?
  [c]
  (and (not (uppercase? c)) (not= c \#)))

(defn error
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn first-of-string
  "FIRST set of string `s` (epsilon as \\#) under `first-sets`."
  [s first-sets]
  (if (or (= s "#") (empty? s))
    #{\#}
    (loop [chars (seq s)
           acc #{}]
      (if (nil? chars)
        (conj acc \#)
        (let [sym (first chars)
              sym-first (get first-sets sym #{})
              no-eps (disj sym-first \#)
              new-acc (into acc no-eps)]
          (if (contains? sym-first \#)
            (recur (next chars) new-acc)
            new-acc))))))

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

(defn compute-first
  "Worklist FIRST sets for `prods` given terminal seed map."
  [prods first-init]
  (let [prods-by-sym
        (reduce (fn [m [idx {:keys [rhs]}]]
                  (if (= rhs "#")
                    m
                    (reduce (fn [m2 sym]
                              (update m2 sym (fnil conj []) idx))
                            m (distinct (seq rhs)))))
                {} (map-indexed vector prods))
        [f0 changed]
        (reduce (fn [[acc ch] {:keys [lhs rhs]}]
                  (let [old (get acc lhs #{})
                        add (if (= rhs "#") #{\#} (first-of-string rhs acc))
                        new (into old add)]
                    (if (= new old)
                      [acc ch]
                      [(assoc acc lhs new) (conj ch lhs)])))
                [first-init #{}]
                prods)]
    (loop [f f0
           q (queue-from changed)
           in-q (set changed)]
      (if (q-empty? q)
        f
        (let [x (q-peek q)
              q (q-pop q)
              in-q (disj in-q x)
              [f q in-q]
              (reduce (fn [[ff qq iq] idx]
                        (let [{:keys [lhs rhs]} (nth prods idx)
                              old (get ff lhs #{})
                              add (if (= rhs "#")
                                    #{\#}
                                    (first-of-string rhs ff))
                              new (into old add)]
                          (if (= new old)
                            [ff qq iq]
                            [(assoc ff lhs new)
                             (if (contains? iq lhs) qq (q-conj qq lhs))
                             (conj iq lhs)])))
                      [f q in-q]
                      (get prods-by-sym x []))]
          (recur f q in-q))))))

(defn compute-follow
  "Worklist FOLLOW sets given final FIRST sets."
  [prods first-sets start-symbol]
  (let [infos
        (vec (for [{:keys [lhs rhs]} prods]
               (if (= rhs "#")
                 {:lhs lhs :positions []}
                 {:lhs lhs
                  :positions
                  (vec (for [i (range (count rhs))
                             :let [B (nth rhs i)]
                             :when (not (terminal? B))
                             :let [rest-str (subs rhs (inc i))
                                   f-rest (first-of-string rest-str first-sets)]]
                         {:sym B
                          :no-eps (disj f-rest \#)
                          :nullable? (contains? f-rest \#)}))})))
        fol0 (reduce (fn [acc {:keys [positions]}]
                       (reduce (fn [a {:keys [sym no-eps]}]
                                 (update a sym (fnil into #{}) no-eps))
                               acc positions))
                     {start-symbol #{\$}}
                     infos)
        prop-edges
        (reduce (fn [m {:keys [lhs positions]}]
                  (reduce (fn [m2 {:keys [sym nullable?]}]
                            (if nullable?
                              (update m2 lhs (fnil conj []) sym)
                              m2))
                          m positions))
                {} infos)
        seeds (filter #(seq (get fol0 %)) (keys prop-edges))
        seeds (if (seq (get fol0 start-symbol)) (conj (set seeds) start-symbol) (set seeds))]
    (loop [fol fol0
           q (queue-from seeds)
           in-q (set seeds)]
      (if (q-empty? q)
        fol
        (let [L (q-peek q)
              q (q-pop q)
              in-q (disj in-q L)
              src (get fol L #{})
              [fol q in-q]
              (reduce (fn [[ff qq iq] B]
                        (let [old (get ff B #{})
                              new (into old src)]
                          (if (= new old)
                            [ff qq iq]
                            [(assoc ff B new)
                             (if (contains? iq B) qq (q-conj qq B))
                             (conj iq B)])))
                      [fol q in-q]
                      (get prop-edges L []))]
          (recur fol q in-q))))))

(defn build-table
  "Build LL(1) table. Returns [table is-ll1?]."
  [prods first-sets follow-sets]
  (reduce
   (fn [[t ok] [idx {:keys [lhs rhs]}]]
     (let [f-rhs (first-of-string rhs first-sets)
           [t1 ok1]
           (reduce
            (fn [[tab status] a]
              (if (= a \#)
                [tab status]
                (let [key [lhs a]]
                  (if (and (contains? tab key) (not= (get tab key) idx))
                    [(assoc tab key idx) false]
                    [(assoc tab key idx) status]))))
            [t ok]
            f-rhs)
           [t2 ok2]
           (if (contains? f-rhs \#)
             (reduce
              (fn [[tab status] b]
                (let [key [lhs b]]
                  (if (and (contains? tab key) (not= (get tab key) idx))
                    [(assoc tab key idx) false]
                    [(assoc tab key idx) status])))
              [t1 ok1]
              (get follow-sets lhs #{}))
             [t1 ok1])]
       [t2 ok2]))
   [{} true]
   (map-indexed vector prods)))

(defn -main
  [& _args]
  (let [s (slurp *in*)]
    (when-not (str/blank? s)
      (let [toks (vec (remove str/blank? (str/split s #"\s+")))
            num-prods (parse-long (get toks 0))]
        (cond
          (nil? num-prods)
          (error "invalid input: expected production count")

          (neg? num-prods)
          (error "invalid input: negative production count")

          (< (count toks) (+' 2 (*' 3 num-prods)))
          (error "invalid input: truncated production list")

          :else
          (let [start-symbol (first (get toks 1))
                prod-toks (take (* 3 num-prods) (drop 2 toks))
                tail (vec (drop (+ 2 (* 3 num-prods)) toks))
                prods (vec (for [[lhs _ rhs] (partition 3 prod-toks)]
                             {:lhs (first lhs) :rhs rhs}))
                terminals (into #{\$}
                                (for [{:keys [rhs]} prods
                                      c rhs
                                      :when (terminal? c)]
                                  c))
                first-init (into {} (for [t terminals] [t #{t}]))
                first-sets (compute-first prods first-init)
                follow-sets (compute-follow prods first-sets start-symbol)
                [table is-ll1] (build-table prods first-sets follow-sets)]
            (if-not is-ll1
              (println "GRAMMAR IS NOT LL1")
              (do
                (println "GRAMMAR IS LL1")
                (when (seq tail)
                  (let [num-queries (parse-long (get tail 0))]
                    (cond
                      (nil? num-queries)
                      (error "invalid input: expected query count")

                      (neg? num-queries)
                      (error "invalid input: negative query count")

                      (< (count (rest tail)) num-queries)
                      (error "invalid input: truncated query list")

                      :else
                      (doseq [w (take num-queries (rest tail))]
                        (let [w (str w "$")
                              n (count w)]
                          (loop [stk [\$ start-symbol]
                                 ip 0
                                 derivations []]
                            (if (empty? stk)
                              (if (= ip n)
                                (println (str/join " " (map #(str "r" %) derivations)))
                                (println "SYNTAX ERROR"))
                              (let [X (peek stk)
                                    stk (pop stk)
                                    a (if (< ip n) (nth w ip) \$)]
                                (cond
                                  (= X a)
                                  (if (= X \$)
                                    (if (= (inc ip) n)
                                      (println (str/join " "
                                                         (map #(str "r" %) derivations)))
                                      (println "SYNTAX ERROR"))
                                    (recur stk (inc ip) derivations))

                                  (terminal? X)
                                  (println "SYNTAX ERROR")

                                  :else
                                  (if-let [rule-idx (get table [X a])]
                                    (let [rhs (:rhs (nth prods rule-idx))
                                          new-stk (if (= rhs "#")
                                                    stk
                                                    (into stk (reverse rhs)))]
                                      (recur new-stk ip (conj derivations rule-idx)))
                                    (println "SYNTAX ERROR")))))))))))))))))))

(-main)
