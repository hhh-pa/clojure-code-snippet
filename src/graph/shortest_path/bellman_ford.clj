;; Bellman Ford single source shortest paths in pure clojure.
;; Relaxing all edges V times costs O(V E) time and O(V + E)
;; space, the best known practical bound for graphs with negative
;; edges, plus cycle tracing when the last pass still relaxes.
;; Parent links and the cycle walk match the old run exactly, so
;; cycle lines print the same. All loops use recur with transient
;; distance and parent tables.
;; Input shape: v e s with 0 based verts, then e lines u v w.
;; Output is NEGATIVE CYCLE plus the cycle line, or one distance
;; line per vertex with INF for unreachable ones. Blank stdin
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
  (let [c (count nums)
        v (nth nums 0 nil)
        e (nth nums 1 nil)
        s (nth nums 2 nil)]
    (if (or (< c 3) (< v 1) (< e 0) (< c (+' 3 (*' e 3)))
            (< s 0) (>= s v))
      (report-error "bad input: want v e s plus e edge triples in range")
      (let [edges (loop [i 0 acc (transient [])]
                    (if (>= i e)
                      (persistent! acc)
                      (let [u (nth nums (+ 3 (* i 3)))
                            to-node (nth nums (+ 4 (* i 3)))
                            w (nth nums (+ 5 (* i 3)))]
                        (if (or (< u 0) (>= u v) (< to-node 0) (>= to-node v))
                          nil
                          (recur (inc i) (conj! acc [u to-node w]))))))]
        (if (nil? edges)
          (report-error "bad input: edge vertex out of range")
          (let [inf 1000000000000000000
                dist (assoc! (transient (vec (repeat v inf))) s 0)
                parent (transient (vec (repeat v -1)))
                [dist parent relaxed]
                (loop [iter 0 dist dist parent parent prev -1]
                  (if (>= iter v)
                    [dist parent prev]
                    (let [[dist parent lr]
                          (loop [es edges dist dist parent parent lr -1]
                            (if (empty? es)
                              [dist parent lr]
                              (let [[u to-node w] (first es)
                                    du (nth dist u)]
                                (if (and (< du inf)
                                         (< (+' du w) (nth dist to-node)))
                                  (recur (rest es)
                                         (assoc! dist to-node (+' du w))
                                         (assoc! parent to-node u)
                                         to-node)
                                  (recur (rest es) dist parent lr)))))]
                      (if (= lr -1)
                        [dist parent -1]
                        (recur (inc iter) dist parent lr)))))]
            (if (not= relaxed -1)
              (let [cn (loop [i 0 cn relaxed]
                         (if (>= i v) cn (recur (inc i) (nth parent cn))))
                    cyc (loop [cur cn acc []]
                          (let [nacc (conj acc cur)]
                            (if (and (= cur cn) (> (count nacc) 1))
                              nacc
                              (recur (nth parent cur) nacc))))]
                (println "NEGATIVE CYCLE")
                (println (clojure.string/join " " (map str (reverse cyc))))
                nil)
              (let [half (quot inf 2)]
                (loop [i 0]
                  (when (< i v)
                    (println (let [d (nth dist i)]
                               (if (>= d half) "INF" (str d))))
                    (recur (inc i))))
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
