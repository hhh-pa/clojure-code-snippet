;; Kruskal minimum spanning forest in pure clojure. Sorting the
;; edges costs O(E log E) time, then union find with path
;; compression and union by size links them in O(E alpha V) time,
;; for O(E log E) time total and O(E) space, the best known
;; practical bound for this problem. The total prints the same for
;; every tie order, since all spanning trees share one weight.
;; Input shape: all integers on stdin as g-nodes g-edges followed
;; by triples u v w with 1 based verts, extra numbers ignored.
;; Output is the forest weight. Blank stdin prints nothing. Badly
;; shaped input prints one message to stderr and returns with exit
;; 0, since a nonzero exit needs host interop, avoided by design.

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

(defn solve
  "Solves one instance from the token vector and prints the answer."
  [nums]
  (let [c (count nums)]
    (if (< c 2)
      (report-error "bad input: want at least node and edge counts")
      (let [g-nodes (nth nums 0)
            g-edges (nth nums 1)]
        (if (or (< g-nodes 0) (< g-edges 0))
          (report-error "bad input: counts must not be negative")
          (let [m (min g-edges (quot (- c 2) 3))
                edges (loop [k 0 acc (transient [])]
                        (if (>= k m)
                          (persistent! acc)
                          (let [u (nth nums (+ 2 (* k 3)))
                                v (nth nums (+ 3 (* k 3)))
                                w (nth nums (+ 4 (* k 3)))]
                            (if (or (< u 1) (> u g-nodes)
                                    (< v 1) (> v g-nodes))
                              nil
                              (recur (inc k)
                                     (conj! acc [(dec u) (dec v) w]))))))]
            (if (nil? edges)
              (report-error "bad input: edge vertex out of range")
              (let [parent (transient (vec (range g-nodes)))
                    sizes (transient (vec (repeat g-nodes 1)))
                    total (loop [es (sort-by #(nth % 2) edges)
                                 parent parent sizes sizes total 0]
                            (if (empty? es)
                              total
                              (let [[x y w] (first es)
                                    [rx parent] (dsu-find parent x)
                                    [ry parent] (dsu-find parent y)]
                                (if (= rx ry)
                                  (recur (rest es) parent sizes total)
                                  (let [sx (nth sizes rx)
                                        sy (nth sizes ry)]
                                    (if (>= sx sy)
                                      (recur (rest es)
                                             (assoc! parent ry rx)
                                             (assoc! sizes rx (+ sx sy))
                                             (+' total w))
                                      (recur (rest es)
                                             (assoc! parent rx ry)
                                             (assoc! sizes ry (+ sx sy))
                                             (+' total w))))))))]
                (println (str total))
                nil))))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
