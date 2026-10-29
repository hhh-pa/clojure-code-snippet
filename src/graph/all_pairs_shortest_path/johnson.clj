;; Johnson all pairs shortest paths in pure clojure. One Bellman Ford
;; run builds vertex potentials in O(V E) time, then one Dijkstra run
;; per source with a sorted set heap costs O(V E log V) time total,
;; which matches the best known practical bound for sparse graphs
;; with negative edges and no negative cycle. Space is O(V + E).
;; Input shape: vertex count, edge count, then triples u v w with 0
;; based vertices. Output is NEGATIVE CYCLE or one distance row per
;; source with INF for unreachable pairs. Blank stdin prints nothing.
;; Badly shaped input prints one message to stderr and returns with
;; exit 0, since a nonzero exit needs host interop, avoided by design.

(require 'clojure.string)

(def digit-values
  "Maps each digit char to its numeric value for the tokenizer below."
  (zipmap "0123456789" (range)))

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


(defn parse-ints
  "Reads all signed integers from string s into a vector. Chars are
   tested with core compare fns after int coercion, digits through
   digit-values, and anything else fails the whole scan with nil."
  [s]
  (let [done
        (reduce
         (fn [state ch]
           (let [nums (:nums state)
                 cur (:cur state)
                 neg (:neg state)
                 bad (:bad state)
                 code (int ch)]
             (cond
               bad state
               (<= (int \0) code (int \9))
               (assoc state :cur (+' (*' (or cur 0) 10) (digit-values ch)))
               (= ch \-)
               (if (or (some? cur) neg)
                 (assoc state :bad true)
                 (assoc state :neg true))
               (<= code (int \space))
               (cond
                 (some? cur)
                 (let [v (fit-long (if neg (-' cur) cur))]
                   (if (some? v)
                     (-> state
                     (assoc :nums (conj! nums v))
                     (assoc :cur nil)
                     (assoc :neg false))
                     (assoc state :bad true)))
                 neg (assoc state :bad true)
                 :else state)
               :else (assoc state :bad true))))
         {:nums (transient []) :cur nil :neg false :bad false}
         (seq s))]
    (when-not (:bad done)
      (let [nums (:nums done)
            cur (:cur done)
            neg (:neg done)]
        (cond
          (some? cur) (let [v (fit-long (if neg (-' cur) cur))]
                        (when (some? v) (persistent! (conj! nums v))))
          neg nil
          :else (persistent! nums))))))

(defn report-error
  "Prints message to stderr and returns nil so the driver ends calmly."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn bellman-ford-potentials
  "Finds potentials h with h set to zero first, as if a zero source
   reached every vertex. Returns the potentials vector, or nil when a
   negative cycle exists. Costs O(V E) time and O(V) extra space."
  [eu ev ew v-count e-count]
  (let [h (transient (vec (repeat v-count 0)))]
    (loop [iter 0 h h]
      (if (>= iter v-count)
        (persistent! h)
        (let [[h changed] (loop [j 0 h h changed false]
                            (if (>= j e-count)
                              [h changed]
                              (let [u (nth eu j)
                                    v (nth ev j)
                                    cand (+' (nth h u) (nth ew j))]
                                (if (< cand (nth h v))
                                  (recur (inc j) (assoc! h v cand) true)
                                  (recur (inc j) h changed)))))]
          (cond
            (not changed) (persistent! h)
            (= iter (dec v-count)) nil
            :else (recur (inc iter) h)))))))

(defn dijkstra-reweighted
  "Shortest reweighted distances from src over adj, where each entry
   holds [to w] pairs and h holds the potentials. A sorted set of
   [dist vertex] pairs acts as the heap in O(log V) per update."
  [adj h src v-count inf]
  (loop [pq (sorted-set [0 src])
         d (assoc! (transient (vec (repeat v-count inf))) src 0)]
    (if (empty? pq)
      (persistent! d)
      (let [[cur-du u] (first pq)
            pq (disj pq [cur-du u])]
        (if (<= cur-du (nth d u))
          (let [hu (nth h u)
                du (nth d u)
                pair (reduce (fn [[d pq] [v w]]
                               (let [cand (+' du (+' w (-' hu (nth h v))))]
                                 (if (< cand (nth d v))
                                   [(assoc! d v cand) (conj pq [cand v])]
                                   [d pq])))
                             [d pq]
                             (nth adj u))]
            (recur (second pair) (first pair)))
          (recur pq d))))))

(defn solve
  "Solves one instance from the token vector and prints the answers."
  [nums]
  (let [c (count nums)
        v-count (nth nums 0 nil)
        e-count (nth nums 1 nil)]
    (if (or (< c 2) (< v-count 0) (< e-count 0) (< c (+' 2 (*' e-count 3))))
      (report-error "bad input: want vertex and edge counts plus edge triples")
      (if (zero? v-count)
        nil
        (let [edges (loop [i 0 acc (transient [])]
                      (if (>= i e-count)
                        (persistent! acc)
                        (let [u (nth nums (+ 2 (* i 3)))
                              v (nth nums (+ 3 (* i 3)))
                              w (nth nums (+ 4 (* i 3)))]
                          (if (or (< u 0) (>= u v-count) (< v 0) (>= v v-count))
                            nil
                            (recur (inc i) (conj! acc [u v w]))))))]
          (if (nil? edges)
            (report-error "bad input: edge vertex out of range")
            (let [eu (mapv first edges)
                  ev (mapv second edges)
                  ew (mapv #(nth % 2) edges)
                  adj (persistent!
                       (reduce (fn [adj [u v w]]
                                 (assoc! adj u (conj (nth adj u) [v w])))
                               (transient (vec (repeat v-count [])))
                               edges))
                  h (bellman-ford-potentials eu ev ew v-count e-count)]
              (if (nil? h)
                (do (println "NEGATIVE CYCLE") nil)
                (let [inf 1000000000000000000
                      half (quot inf 2)
                      rows (vec
                            (for [src (range v-count)]
                              (let [d (dijkstra-reweighted adj h src v-count inf)
                                    hs (nth h src)]
                                (clojure.string/join
                                 " "
                                 (map (fn [v]
                                        (let [dist (nth d v)]
                                          (if (>= dist half)
                                            "INF"
                                            (str (+' dist (-' (nth h v) hs))))))
                                      (range v-count))))))]
                  (println (clojure.string/join "\n" rows))
                  nil)))))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
