;; Boruvka minimum spanning tree in pure clojure. Each phase
;; scans all edges once to find the cheapest edge out of every
;; component, then merges through a disjoint set with path halving
;; and union by rank, so phases cost O(E alpha V) each and the
;; component count at least halves per phase in connected graphs,
;; for O(E log V) time total and O(V + E) space, the best known
;; practical bound shared with the Kruskal and Prim methods. The
;; old recursive find is replaced by an iterative walk with the
;; same bound. Input shape: n m, then m lines u v w with 0 based
;; verts. Output is the tree weight or the word DISCONNECTED for
;; split graphs. Blank stdin prints nothing.
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

(defn dsu-find
  "Finds the root of x with full path compression, walking up first
   and then pointing the whole path at the root. Returns the pair
   of root and parent table. Costs inverse Ackermann amortized time."
  [parent x]
  (let [root (loop [x x]
               (let [p (nth parent x)]
                 (if (= p x) x (recur p))))]
    [root (loop [x x parent parent]
            (let [p (nth parent x)]
              (if (= p root)
                parent
                (recur p (assoc! parent x root)))))]))

(defn dsu-unite
  "Merges the sets of a and b by rank. Returns the tuple of merged
   flag, parent table, and rank table."
  [parent rank a b]
  (let [[ra parent] (dsu-find parent a)
        [rb parent] (dsu-find parent b)]
    (if (= ra rb)
      [false parent rank]
      (let [rka (nth rank ra)
            rkb (nth rank rb)]
        (cond
          (< rka rkb) [true (assoc! parent ra rb) rank]
          (> rka rkb) [true (assoc! parent rb ra) rank]
          :else [true
                 (assoc! parent rb ra)
                 (assoc! rank ra (inc rka))])))))

(defn solve
  "Solves one instance from the token vector and prints the answer."
  [nums]
  (let [c (count nums)
        n (nth nums 0 nil)
        m (nth nums 1 nil)]
    (if (or (< c 2) (< n 0) (< m 0) (< c (+' 2 (*' m 3))))
      (report-error "bad input: want n m plus m edge triples")
      (if (zero? n)
        (do (println "DISCONNECTED") nil)
        (let [edges (loop [i 0 acc (transient [])]
                      (if (>= i m)
                        (persistent! acc)
                        (let [u (nth nums (+ 2 (* i 3)))
                              v (nth nums (+ 3 (* i 3)))
                              w (nth nums (+ 4 (* i 3)))]
                          (if (or (< u 0) (>= u n) (< v 0) (>= v n))
                            nil
                            (recur (inc i) (conj! acc [u v w]))))))]
          (if (nil? edges)
            (report-error "bad input: edge vertex out of range")
            (let [eu (mapv first edges)
                  ev (mapv second edges)
                  ew (mapv #(nth % 2) edges)
                  parent (transient (vec (range n)))
                  rank (transient (vec (repeat n 0)))]
              (loop [weight 0 added 0 parent parent rank rank]
                (if (= added (dec n))
                  (do (println (str weight)) nil)
                  (let [cheapest (loop [i 0
                                        cheapest (transient (vec (repeat n -1)))
                                        parent parent]
                                   (if (>= i m)
                                     [cheapest parent]
                                     (let [[ru parent] (dsu-find parent (nth eu i))
                                           [rv parent] (dsu-find parent (nth ev i))]
                                       (if (= ru rv)
                                         (recur (inc i) cheapest parent)
                                         (let [cu (nth cheapest ru)
                                               cheapest (if (or (= cu -1)
                                                                (< (nth ew i)
                                                                   (nth ew cu)))
                                                          (assoc! cheapest ru i)
                                                          cheapest)
                                               cv (nth cheapest rv)
                                               cheapest (if (or (= cv -1)
                                                                (< (nth ew i)
                                                                   (nth ew cv)))
                                                          (assoc! cheapest rv i)
                                                          cheapest)]
                                           (recur (inc i) cheapest parent))))))
                        [cheapest parent] cheapest
                        [weight added progress parent rank]
                        (loop [i 0 weight weight added added progress false
                               parent parent rank rank]
                          (if (>= i n)
                            [weight added progress parent rank]
                            (let [e (nth cheapest i)]
                              (if (= e -1)
                                (recur (inc i) weight added progress
                                       parent rank)
                                (let [[merged parent rank]
                                      (dsu-unite parent rank
                                                 (nth eu e) (nth ev e))]
                                  (if merged
                                    (recur (inc i) (+' weight (nth ew e))
                                           (inc added) true parent rank)
                                    (recur (inc i) weight added progress
                                           parent rank)))))))]
                    (if progress
                      (recur weight added parent rank)
                      (do (println "DISCONNECTED") nil))))))))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
