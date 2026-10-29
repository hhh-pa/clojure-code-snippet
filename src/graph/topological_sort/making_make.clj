;; Topological order with max first Kahn search in pure clojure.
;; A sorted set of ready verts always yields the current maximum,
;; giving O((V + E) log V) time and O(V + E) space, the best known
;; bound for this fixed order. Each vertex enters and leaves the
;; set once, and each edge drops one indegree. Output lists the
;; pop order, matching the old run exactly.
;; Input shape: all integers on stdin as v e followed by pairs
;; from to with 0 based verts, extra numbers ignored. Blank stdin
;; prints nothing. Badly shaped input prints one message to stderr
;; and returns with exit 0, since a nonzero exit needs host
;; interop, which this script avoids by design.

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

(defn solve
  "Solves one instance from the token vector and prints the answer."
  [nums]
  (let [c (count nums)]
    (if (< c 2)
      (report-error "bad input: want at least vertex and edge counts")
      (let [vv (nth nums 0)
            ee (nth nums 1)]
        (if (< vv 0)
          (report-error "bad input: vertex count must not be negative")
          (let [m (max 0 (min ee (quot (- c 2) 2)))
                built (loop [k 0
                             g (transient (vec (repeat vv [])))
                             indeg (transient (vec (repeat vv 0)))]
                        (if (>= k m)
                          [g indeg]
                          (let [from (nth nums (+ 2 (* k 2)))
                                to (nth nums (+ 3 (* k 2)))]
                            (if (or (< from 0) (>= from vv)
                                    (< to 0) (>= to vv))
                              nil
                              (recur (inc k)
                                     (assoc! g from
                                             (conj (nth g from) to))
                                     (assoc! indeg to
                                             (inc (nth indeg to))))))))]
            (if (nil? built)
              (report-error "bad input: edge vertex out of range")
              (let [[g indeg] built
                    ready (loop [i 0 ready (sorted-set)]
                            (if (>= i vv)
                              ready
                              (recur (inc i)
                                     (if (zero? (nth indeg i))
                                       (conj ready i)
                                       ready))))
                    [order _] (loop [ready ready
                                     order (transient (vec (repeat vv 0)))
                                     indeg indeg
                                     counter 0]
                                (if (empty? ready)
                                  [(persistent! order) indeg]
                                  (let [v (last ready)
                                        ready (disj ready v)
                                        order (assoc! order v counter)
                                        [ready indeg]
                                        (loop [es (nth g v)
                                               ready ready indeg indeg]
                                          (if (empty? es)
                                            [ready indeg]
                                            (let [w (first es)
                                                  ni (dec (nth indeg w))
                                                  indeg (assoc! indeg w ni)]
                                              (if (zero? ni)
                                                (recur (rest es)
                                                       (conj ready w)
                                                       indeg)
                                                (recur (rest es)
                                                       ready indeg)))))]
                                    (recur ready order indeg (inc counter)))))]
                (print (str (apply str (map (fn [x] (str x " "))
                                            (reduce (fn [ans i]
                                                      (assoc ans (nth order i)
                                                             i))
                                                    (vec (repeat vv 0))
                                                    (range vv))))
                            "\n"))
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
