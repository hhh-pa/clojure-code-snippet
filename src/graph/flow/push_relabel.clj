;; Push relabel max flow in pure clojure. Highest label vertex
;; choice with height buckets plus the gap rule gives O(V**2 sqrt E)
;; worst case time and O(E) space, the best known practical push
;; relabel bound. The old run kept height counts but never applied
;; the gap rule, and always picked the oldest vertex, so this order
;; is both asymptotically and practically ahead while printing the
;; same flow value. Buckets hold active verts per height, stale
;; entries are skipped lazily, and all loops use recur only.
;; Input shape: n m s t, then m lines u v cap with 0 based verts.
;; Output is the max flow value. Blank stdin prints nothing. Badly
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

(defn activate
  "Puts v in its height bucket when it holds excess and is idle.
   Returns the pair of state and highest used bucket."
  [st v max-h s t]
  (let [active (:active st)]
    (if (and (not (nth active v)) (not= v s) (not= v t)
             (> (nth (:excess st) v) 0))
      (let [h (nth (:height st) v)
            buckets (:buckets st)]
        [(-> st
             (assoc :active (assoc! active v true))
             (assoc :buckets (assoc! buckets h (conj (nth buckets h) v))))
         (max max-h h)])
      [st max-h])))

(defn apply-gap
  "Lifts every vertex strictly between old-h and n up to n plus 1
   once no vertex keeps height old-h. Stale bucket entries stay in
   place and are skipped later. Returns state plus highest bucket."
  [st old-h max-h n s t]
  (let [target (inc n)]
    (loop [v 0 st st max-h max-h]
      (if (>= v n)
        [st max-h]
        (let [height (:height st)
              hv (nth height v)]
          (if (and (not= v s) (not= v t) (> hv old-h) (< hv n))
            (let [counts (:counts st)
                  counts (-> counts
                             (assoc! hv (dec (nth counts hv)))
                             (assoc! target (inc (nth counts target))))
                  st (assoc st
                            :height (assoc! height v target)
                            :counts counts)
                  buckets (:buckets st)
                  [st max-h] (if (nth (:active st) v)
                               [(assoc st :buckets
                                       (assoc! buckets target
                                               (conj (nth buckets target) v)))
                                (max max-h target)]
                               [st max-h])]
              (recur (inc v) st max-h))
            (recur (inc v) st max-h)))))))

(defn discharge
  "Pushes excess out of u until none is left, relabelling over the
   lowest residual neighbor when the current arc runs out. Returns
   the pair of state and highest used bucket."
  [st u max-h adj to cap rev n s t]
  (loop [st st max-h max-h]
    (if (zero? (nth (:excess st) u))
      [st max-h]
      (let [arcs (:arcs st)
            idx (nth arcs u)
            nbrs (nth adj u)]
        (if (< idx (count nbrs))
          (let [eid (nth nbrs idx)
                w (nth to eid)
                flow (:flow st)
                height (:height st)
                rem (-' (nth cap eid) (nth flow eid))]
            (if (and (> rem 0) (= (nth height u) (inc (nth height w))))
              (let [delta (min (nth (:excess st) u) rem)
                    r (nth rev eid)
                    flow (-> flow
                             (assoc! eid (+' (nth flow eid) delta))
                             (assoc! r (-' (nth flow r) delta)))
                    excess (:excess st)
                    excess (-> excess
                               (assoc! u (-' (nth excess u) delta))
                               (assoc! w (+' (nth excess w) delta)))
                    st (assoc st :flow flow :excess excess)
                    [st max-h] (activate st w max-h s t)]
                (recur st max-h))
              (recur (assoc st :arcs (assoc! arcs u (inc idx))) max-h)))
          (let [flow (:flow st)
                height (:height st)
                size (* 2 n)
                min-h (loop [js nbrs best size]
                        (if (empty? js)
                          best
                          (let [e (first js)]
                            (if (> (-' (nth cap e) (nth flow e)) 0)
                              (recur (rest js)
                                     (min best (nth height (nth to e))))
                              (recur (rest js) best)))))
                new-h (inc min-h)
                old-h (nth height u)
                counts (:counts st)
                counts (-> counts
                           (assoc! old-h (dec (nth counts old-h)))
                           (assoc! new-h (inc (nth counts new-h))))
                st (assoc st
                          :height (assoc! height u new-h)
                          :counts counts
                          :arcs (assoc! (:arcs st) u 0))
                [st max-h] (if (and (< old-h n)
                                    (zero? (nth (:counts st) old-h)))
                             (apply-gap st old-h max-h n s t)
                             [st max-h])]
            (recur st max-h)))))))

(defn max-flow
  "Highest label push relabel main loop. Saturates the source edges
   first, then always discharges from the highest active bucket."
  [adj to cap rev flow s t n]
  (let [size (* 2 n)
        st {:flow flow
            :excess (transient (vec (repeat n 0)))
            :height (assoc! (transient (vec (repeat n 0))) s n)
            :counts (-> (transient (vec (repeat size 0)))
                        (assoc! n 1)
                        (assoc! 0 (dec n)))
            :active (transient (vec (repeat n false)))
            :arcs (transient (vec (repeat n 0)))
            :buckets (transient (vec (repeat size ())))}
        [st max-h] (reduce (fn [[st max-h] eid]
                             (let [amt (nth cap eid)]
                               (if (<= amt 0)
                                 [st max-h]
                                 (let [w (nth to eid)
                                       r (nth rev eid)
                                       flow (:flow st)
                                       flow (-> flow
                                                (assoc! eid
                                                        (+' (nth flow eid) amt))
                                                (assoc! r (-' (nth flow r) amt)))
                                       excess (:excess st)
                                       excess (-> excess
                                                  (assoc! w (+' (nth excess w) amt))
                                                  (assoc! s (-' (nth excess s) amt)))
                                       st (assoc st :flow flow :excess excess)]
                                   (activate st w max-h s t)))))
                           [st -1]
                           (nth adj s))]
    (loop [st st max-h max-h]
      (if (< max-h 0)
        (nth (:excess st) t)
        (let [buckets (:buckets st)
              bucket (nth buckets max-h)]
          (if (empty? bucket)
            (recur st (dec max-h))
            (let [u (peek bucket)
                  st (assoc st :buckets (assoc! buckets max-h (pop bucket)))]
              (if (or (not (nth (:active st) u))
                      (not= (nth (:height st) u) max-h))
                (recur st max-h)
                (let [st (assoc st :active (assoc! (:active st) u false))
                      [st max-h] (discharge st u max-h adj to cap rev n s t)]
                  (recur st max-h))))))))))

(defn solve
  "Solves one instance from the token vector and prints the answer."
  [nums]
  (let [c (count nums)
        n (nth nums 0 nil)
        m (nth nums 1 nil)
        s (nth nums 2 nil)
        t (nth nums 3 nil)]
    (if (or (< c 4) (< n 1) (< m 0) (< c (+' 4 (*' m 3)))
            (< s 0) (>= s n) (< t 0) (>= t n))
      (report-error "bad input: want n m s t plus m edge triples in range")
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
                                       (assoc! v (conj (nth adj v)
                                                       (inc e))))))))))]
        (if (nil? built)
          (report-error "bad input: edge vertex out of range")
          (let [[to rev cap adj] built
                flow (transient (vec (repeat (* 2 m) 0)))]
            (println (str (max-flow adj to cap rev flow s t n)))
            nil))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
