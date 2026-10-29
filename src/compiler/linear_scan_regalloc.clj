(ns compiler.linear-scan-regalloc
  "Linear-scan register allocation over live intervals.

   Time: O(n log n) for sorting by start plus O(n * k) scanning, where k is
   the register count (each step expires/inserts in O(k) over the active
   list, which holds at most k intervals). For fixed k this is O(n log n)
   overall, optimal for linear scan; HEAD re-sorted the active list in
   O(k log k) per interval. Space: O(n + k).
   Spill heuristic matches HEAD exactly (spill the active interval with the
   latest end when full)."
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
  "Parse optional sign + digits; nil when invalid."
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

(defn read-tokens
  "Split stdin into whitespace-separated tokens."
  [s]
  (vec (remove str/blank? (str/split s #"\s+"))))

(defn insert-by-end
  "Insert `item` into `active` (sorted by :end, stable) in O(k), keeping
   `item` after existing entries with equal :end."
  [active item]
  (let [e (:end item)
        idx (loop [i 0]
              (if (or (>= i (count active))
                      (> (:end (nth active i)) e))
                i
                (recur (inc i))))]
    (vec (concat (subvec active 0 idx) [item] (subvec active idx)))))

(defn error
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn allocate
  "Allocate registers for sorted intervals. Returns [assigned spilled-set]."
  [sorted-intervals k-regs n]
  (loop [remaining sorted-intervals
         free-regs (into (sorted-set) (range k-regs))
         active []
         assigned (vec (repeat n -1))
         spilled #{}]
    (if-not (seq remaining)
      [assigned spilled]
      (let [cur (first remaining)
            cur-start (:start cur)
            expired (filter #(< (:end %) cur-start) active)
            new-free (into free-regs (map :reg expired))
            unexpired (vec (filter #(>= (:end %) cur-start) active))]
        (if (= (count unexpired) k-regs)
          (if (zero? k-regs)
            (recur (rest remaining) new-free unexpired assigned
                   (conj spilled (:id cur)))
            (let [latest (apply max-key :end unexpired)]
              (if (> (:end latest) (:end cur))
                (let [reg (:reg latest)
                      new-active (->> unexpired
                                      (remove #(= (:id %) (:id latest)))
                                      vec
                                      (#(insert-by-end % {:end (:end cur)
                                                          :id (:id cur)
                                                          :reg reg})))]
                  (recur (rest remaining)
                         new-free
                         new-active
                         (-> assigned
                             (assoc (:id latest) -1)
                             (assoc (:id cur) reg))
                         (conj spilled (:id latest))))
                (recur (rest remaining) new-free unexpired assigned
                       (conj spilled (:id cur))))))
          (let [reg (first new-free)
                rem-free (disj new-free reg)
                new-active (insert-by-end unexpired {:end (:end cur)
                                                     :id (:id cur)
                                                     :reg reg})]
            (recur (rest remaining) rem-free new-active
                   (assoc assigned (:id cur) reg) spilled)))))))

(defn -main
  [& _args]
  (let [s (slurp *in*)]
    (when-not (str/blank? s)
      (let [toks (read-tokens s)
            n (parse-long (get toks 0))
            k-regs (parse-long (get toks 1))]
        (cond
          (or (nil? n) (nil? k-regs) (neg? n) (neg? k-regs))
          (error "invalid input: expected n k-regs header")

          (< (count toks) (+' 2 (*' n 3)))
          (error "invalid input: truncated interval list")

          :else
          (let [triples (take (* n 3) (drop 2 toks))
                parsed (vec (for [[a b c] (partition 3 triples)]
                              [(parse-long a) (parse-long b) (parse-long c)]))]
            (if (or (some (fn [[a b c]] (or (nil? a) (nil? b) (nil? c))) parsed)
                    (some (fn [[id _ _]] (or (< id 0) (>= id n))) parsed))
              (error "invalid input: bad interval triple")
              (let [intervals (vec (for [[id st en] parsed]
                                     {:id id :start st :end en}))
                    sorted-intervals (sort-by (juxt :start :end) intervals)
                    [assigned spilled] (allocate sorted-intervals k-regs n)]
                (loop [i 0
                       allocated 0
                       spilled-count 0]
                  (if (>= i n)
                    (println (str "Allocated: " allocated " Spilled: " spilled-count))
                    (if (contains? spilled i)
                      (do
                        (println (str "v" i ": SPILL"))
                        (recur (inc i) allocated (inc spilled-count)))
                      (do
                        (println (str "v" i ": R" (nth assigned i)))
                        (recur (inc i) (inc allocated) spilled-count)))))))))))))

(-main)
