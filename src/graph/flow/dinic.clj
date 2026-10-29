;; Dinic max flow in pure clojure. Each phase builds the level
;; graph with one breadth first search in O(E) time, then pushes a
;; blocking flow with repeated explicit stack depth first walks that
;; keep current arc pointers, for O(V E) time per phase and O(V**2 E)
;; time total with O(E) space, matching the best known practical
;; Dinic bound. The old recursive walk is replaced by an explicit
;; stack over the same level graph, so deep graphs cannot overflow
;; the call stack while the complexity stays the same.
;; Input shape: n m s t, then m lines u v cap with 0 based verts.
;; Output is the max flow value. Blank stdin prints nothing. Badly
;; shaped input, including s equal to t, prints one message to
;; stderr and returns with exit 0, since a nonzero exit needs host
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

(defn bfs-levels
  "Builds Dinic levels from s over the residual graph. adj maps each
   vertex to its edge id vector, to and cap hold edge targets and
   caps, flow holds current flows. Returns the level vector, or nil
   when t stays unreachable. Costs O(E) time and O(V) extra space."
  [adj to cap flow s t n]
  (let [level (assoc! (transient (vec (repeat n -1))) s 0)]
    (loop [queue (conj [] s) head 0 level level]
      (if (>= head (count queue))
        (when (not= (nth level t) -1) (persistent! level))
        (let [v (nth queue head)
              lv (nth level v)
              pair (reduce (fn [[queue level] eid]
                             (let [w (nth to eid)
                                   rem (- (nth cap eid) (nth flow eid))]
                               (if (and (> rem 0) (= (nth level w) -1))
                                 [(conj queue w) (assoc! level w (inc lv))]
                                 [queue level])))
                           [queue level]
                           (nth adj v))]
          (recur (first pair) (inc head) (second pair)))))))

(defn augment-path
  "Finds one s t path in the level graph with current arcs ptr and
   pushes its bottleneck flow. Uses an explicit stack of vertices
   plus the edge ids that reached them. Returns [pushed flow ptr]
   with pushed 0 when no path is left. Each call costs O(V + E)."
  [adj to cap rev flow level ptr s t]
  (loop [vstack [s] estack [] flow flow ptr ptr]
    (if (empty? vstack)
      [0 flow ptr]
      (let [v (peek vstack)]
        (if (= v t)
          (let [e0 (first estack)
                neck (reduce (fn [best eid]
                               (min best (- (nth cap eid) (nth flow eid))))
                             (- (nth cap e0) (nth flow e0))
                             (rest estack))
                flow (reduce (fn [flow eid]
                               (-> flow
                                   (assoc! eid (+ (nth flow eid) neck))
                                   (assoc! (nth rev eid)
                                           (- (nth flow (nth rev eid)) neck))))
                             flow
                             estack)]
            [neck flow ptr])
          (let [nbrs (nth adj v)
                i (nth ptr v)]
            (if (>= i (count nbrs))
              (if (= (count vstack) 1)
                (recur (pop vstack) estack flow ptr)
                (recur (pop vstack) (pop estack) flow ptr))
              (let [eid (nth nbrs i)
                    w (nth to eid)
                    rem (- (nth cap eid) (nth flow eid))
                    ptr (assoc! ptr v (inc i))]
                (if (and (> rem 0) (= (nth level w) (inc (nth level v))))
                  (recur (conj vstack w) (conj estack eid) flow ptr)
                  (recur vstack estack flow ptr))))))))))

(defn max-flow
  "Dinic main loop. Rebuilds levels and current arcs per phase and
   pushes paths until t is cut off. Returns the flow value."
  [adj to cap rev flow s t n]
  (loop [flow flow total 0]
    (let [level (bfs-levels adj to cap flow s t n)]
      (if (nil? level)
        total
        (let [ptr (transient (vec (repeat n 0)))
              [flow pushed] (loop [flow flow pushed 0 ptr ptr]
                              (let [[got flow ptr] (augment-path adj to cap rev
                                                                flow level ptr s t)]
                                (if (zero? got)
                                  [flow pushed]
                                  (recur flow (+' pushed got) ptr))))]
          (recur flow (+' total pushed)))))))

(defn solve
  "Solves one instance from the token vector and prints the answer."
  [nums]
  (let [c (count nums)]
    (if (< c 4)
      (report-error "bad input: want n m s t plus m edge triples with s not t")
      (let [n (nth nums 0)
            m (nth nums 1)
            s (nth nums 2)
            t (nth nums 3)]
        (if (or (< n 1) (< m 0) (< c (+' 4 (*' m 3)))
            (< s 0) (>= s n) (< t 0) (>= t n) (= s t))
      (report-error "bad input: want n m s t plus m edge triples with s not t")
      (let [built (loop [i 0
                         to (transient [])
                         rev (transient [])
                         cap (transient [])
                         adj (transient (vec (repeat n [])))]
                    (if (>= i m)
                      [(persistent! to) (persistent! rev)
                       (persistent! cap) (persistent! adj)]
                      (let [u (nth nums (+ 4 (* i 3)))
                            v (nth nums (+ 5 (* i 3)))
                            w (nth nums (+ 6 (* i 3)))]
                        (if (or (< u 0) (>= u n) (< v 0) (>= v n))
                          nil
                          (let [e (* i 2)]
                            (recur (inc i)
                                   (-> to (conj! v) (conj! u))
                                   (-> rev (conj! (inc e)) (conj! e))
                                   (-> cap (conj! w) (conj! 0))
                                   (-> adj
                                       (assoc! u (conj (nth adj u) e))
                                       (assoc! v (conj (nth adj v) (inc e))))))))))]
        (if (nil? built)
          (report-error "bad input: edge vertex out of range")
          (let [[to rev cap adj] built
                flow (transient (vec (repeat (* 2 m) 0)))]
            (println (str (max-flow adj to cap rev flow s t n)))
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
