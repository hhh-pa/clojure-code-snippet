;; Centralized minimum spanning tree via Kruskal, in the GHS spirit.
;; Reads n m, then m edges as u v w triples.
;; Sorting dominates: O(m log m) time, while union find with union by size
;; plus path compression keeps each query near constant, using O(n + m) space.
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

(defn find-root
  "Find the set root of x, compressing the path. Returns [root parent]."
  [parent x]
  (loop [cur x, path []]
    (let [p (nth parent cur)]
      (if (= p cur)
        [cur (reduce (fn [par node] (assoc par node cur)) parent path)]
        (recur p (conj path cur))))))

(defn unite
  "Union the sets of a and b by size. Returns [merged parent size]."
  [parent size a b]
  (let [[ra parent1] (find-root parent a)
        [rb parent2] (find-root parent1 b)]
    (if (= ra rb)
      [false parent2 size]
      (if (>= (nth size ra) (nth size rb))
        [true (assoc parent2 rb ra) (assoc size ra (+ (nth size ra) (nth size rb)))]
        [true (assoc parent2 ra rb) (assoc size rb (+ (nth size ra) (nth size rb)))]))))

(defn kruskal
  "Kruskal MST over edges sorted by weight. Returns [weight count pairs]."
  [edges n]
  (loop [es edges
         parent (vec (range n))
         size (vec (repeat n 1))
         weight 0
         cnt 0
         pairs []]
    (if (empty? es)
      [weight cnt pairs]
      (let [{:keys [u v w]} (first es)
            [merged parent2 size2] (unite parent size u v)]
        (if merged
          (recur (rest es) parent2 size2 (+' weight w) (inc cnt)
                 (conj pairs [(min u v) (max u v)]))
          (recur (rest es) parent2 size2 weight cnt pairs))))))

(defn valid-endpoint?
  "True when p names one of n vertices."
  [n p]
  (and (some? p) (integer? p) (<= 0 p) (< p n)))

(defn simulate
  "Build output lines from toks, or fail to stderr and return nil."
  [toks]
  (if (< (count toks) 2)
    (fail! "Invalid input: expected n m then m u v w triples")
    (let [n (parse-int (nth toks 0))
          m (parse-int (nth toks 1))]
      (cond
        (or (nil? n) (neg? n))
        (fail! "Invalid input: n must be a non-negative integer")
        (or (nil? m) (neg? m))
        (fail! "Invalid input: m must be a non-negative integer")
        (< (count toks) (+' 2 (*' 3 m)))
        (fail! "Invalid input: fewer edge tokens than m claims")
        :else
        (let [edges (vec (for [i (range m)]
                           {:u (parse-int (nth toks (+ 2 (* 3 i))))
                            :v (parse-int (nth toks (+ 3 (* 3 i))))
                            :w (parse-int (nth toks (+ 4 (* 3 i))))}))]
          (cond
            (some #(or (nil? (:u %)) (nil? (:v %)) (nil? (:w %))) edges)
            (fail! "Invalid input: edge endpoints and weights must be integers")
            (some #(or (not (valid-endpoint? n (:u %)))
                       (not (valid-endpoint? n (:v %))))
                  edges)
            (fail! "Invalid input: edge endpoint out of range")
            :else
            (let [[weight cnt pairs] (kruskal (sort-by :w edges) n)]
              (into [(str "GHS MST Weight: " weight)
                     (str "Edges: " cnt)]
                    (map (fn [[a b]] (str a " " b)))
                    (sort pairs)))))))))

(defn -main
  "Entry point: tokenize INPUT and print result lines. Blank input prints
   nothing. Invalid input prints a note to stderr with no stdout."
  [input]
  (when-not (str/blank? input)
    (let [lines (simulate (tokenize input))]
      (when (some? lines)
        (run! println lines)))))

(-main (slurp *in*))
