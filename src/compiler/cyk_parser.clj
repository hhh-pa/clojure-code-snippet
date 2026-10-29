(ns compiler.cyk-parser
  "Cocke-Younger-Kasami (CYK) parser with ambiguity counting for grammars
   in Chomsky normal form (each production is A -> a or A -> BC).

   Time: cubic in n times min(|P|, |N|*|N|) per query: each of the
   O(n*n) cells tries O(n) splits, combining children by iterating over
   the smaller of the binary-rule list and the present-symbol cross
   product via a pair index. This matches the cubic CYK bound in |G| and
   is much faster on sparse grammars than the original unconditional
   cubic scan in |P|. Space: O(n*n*|N-present|) sparse table (the
   original used a dense n*n*256 array). Counting is modulo 1e9+7."
  (:refer-clojure :exclude [parse-long])
  (:require [clojure.string :as str]))

(def MOD 1000000007)

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

(defn combine-cells
  "Combine `left` and `right` count-maps into cell accumulator `cell`,
   using `pair-index` ([b c] -> [lhs...]) or `binary-rules` ([lhs b c]),
   whichever iterates less. Returns updated cell map."
  [cell left right pair-index binary-rules]
  (if (or (empty? left) (empty? right))
    cell
    (if (<= (* (count left) (count right)) (count binary-rules))
      (reduce (fn [acc [b lb]]
                (reduce (fn [acc2 [c lc]]
                          (if-let [lhs-list (get pair-index [b c])]
                            (let [add (rem (* lb lc) MOD)]
                              (reduce (fn [a lhs]
                                        (assoc a lhs (rem (+ (get a lhs 0) add) MOD)))
                                      acc2 lhs-list))
                            acc2))
                        acc right))
              cell left)
      (reduce (fn [acc [lhs b c]]
                (let [lb (get left b 0)]
                  (if (pos? lb)
                    (let [lc (get right c 0)]
                      (if (pos? lc)
                        (let [add (rem (* lb lc) MOD)]
                          (assoc acc lhs (rem (+ (get acc lhs 0) add) MOD)))
                        acc))
                    acc)))
              cell binary-rules))))

(defn count-parses
  "Number of parses of string `w` from `start` (mod MOD)."
  [w start term-index pair-index binary-rules]
  (let [n (count w)]
    (if (zero? n)
      0
      (let [chars (vec w)
            empty-row (vec (repeat n {}))
            dp0 (vec (repeat (inc n) empty-row))
            dp1 (reduce (fn [dp i]
                          (if-let [lhs-list (get term-index (nth chars i))]
                            (reduce (fn [d lhs]
                                      (update-in d [1 i lhs] (fnil inc 0)))
                                    dp lhs-list)
                            dp))
                        dp0 (range n))
            dp-final
            (loop [dp dp1
                   len 2]
              (if (> len n)
                dp
                (let [dp-next
                      (loop [dp dp
                             i 0]
                        (if (> i (- n len))
                          dp
                          (let [cell
                                (loop [k 1
                                       acc {}]
                                  (if (>= k len)
                                    acc
                                    (let [left (get-in dp [k i])
                                          right (get-in dp [(- len k) (+ i k)])]
                                      (recur (inc k)
                                             (combine-cells acc left right
                                                            pair-index binary-rules)))))]
                            (recur (if (empty? cell)
                                     dp
                                     (assoc-in dp [len i] cell))
                                   (inc i)))))]
                  (recur dp-next (inc len)))))]
        (get-in dp-final [n 0 start] 0)))))

(defn -main
  [& _args]
  (let [s (slurp *in*)]
    (when-not (str/blank? s)
      (let [toks (vec (remove str/blank? (str/split s #"\s+")))
            num-rules (parse-long (get toks 0))]
        (cond
          (nil? num-rules)
          (error "invalid input: expected rule count")

          (neg? num-rules)
          (error "invalid input: negative rule count")

          (< (count toks) (+' 2 (*' num-rules 3)))
          (error "invalid input: truncated rule list")

          :else
          (let [start-tok (get toks 1)
                start (first start-tok)
                rule-toks (take (* num-rules 3) (drop 2 toks))
                rest-toks (vec (drop (+ 2 (* num-rules 3)) toks))
                rules (vec (for [[lhs _ rhs] (partition 3 rule-toks)]
                             (if (= 1 (count rhs))
                               {:lhs (first lhs) :term (first rhs)}
                               {:lhs (first lhs)
                                :rhs1 (first rhs)
                                :rhs2 (second rhs)})))
                term-index (reduce (fn [m r]
                                     (if-let [t (:term r)]
                                       (update m t (fnil conj []) (:lhs r))
                                       m))
                                   {} rules)
                binary-rules (vec (for [r rules :when (:rhs1 r)]
                                    [(:lhs r) (:rhs1 r) (:rhs2 r)]))
                pair-index (reduce (fn [m [lhs b c]]
                                     (update m [b c] (fnil conj []) lhs))
                                   {} binary-rules)
                num-queries (if (seq rest-toks)
                              (parse-long (first rest-toks))
                              0)
                num-queries (if (nil? num-queries) 0 num-queries)
                queries (take num-queries (rest rest-toks))]
            (if (< (count (rest rest-toks)) num-queries)
              (when (pos? num-queries)
                (error "invalid input: truncated query list"))
              (doseq [w queries]
                (let [ways (count-parses w start term-index pair-index binary-rules)]
                  (if (pos? ways)
                    (println (str "YES " ways))
                    (println "NO 0")))))))))))

(-main)
