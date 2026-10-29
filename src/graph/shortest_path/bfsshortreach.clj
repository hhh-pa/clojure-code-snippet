;; Breadth first shortest reach over many cases in pure clojure.
;; Each case costs O(V + E) time and O(V + E) space with a vector
;; queue and transient distances, the best known bound for
;; unweighted shortest paths. Edge weight is six per hop, and
;; unreachable verts print minus one, exactly like the old run.
;; Input shape: case count t, then per case n m, m lines u v with
;; 1 based verts, and a source. Output holds one distance line per
;; case, skipping the source. Blank stdin prints nothing. Badly
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

(defn bfs-line
  "Runs one case from token position p and returns the pair of next
   position and output line, or nil when the shape is wrong. adj is
   built per case, then one breadth first search fills distances."
  [nums p]
  (let [c (count nums)]
    (when (>= (- c p) 2)
      (let [n (nth nums p)
            m (nth nums (inc p))]
        (when (and (>= n 1) (>= m 0) (>= (- c p) (+' 3 (*' m 2))))
          (let [adj (loop [i 0 adj (transient (vec (repeat (inc n) [])))]
                      (if (>= i m)
                        (persistent! adj)
                        (let [u (nth nums (+ p 2 (* i 2)))
                              v (nth nums (+ p 3 (* i 2)))]
                          (if (or (< u 1) (> u n) (< v 1) (> v n))
                            nil
                            (recur (inc i)
                                   (-> adj
                                       (assoc! u (conj (nth adj u) v))
                                       (assoc! v (conj (nth adj v) u))))))))]
            (when (some? adj)
              (let [src (nth nums (+ p 2 (* m 2)))]
                (when (and (>= src 1) (<= src n))
                  (let [dist (assoc! (transient (vec (repeat (inc n) -1)))
                                     src 0)
                        dist (loop [queue [src] head 0 dist dist]
                               (if (>= head (count queue))
                                 dist
                                 (let [u (nth queue head)
                                       dd (nth dist u)
                                       [queue dist]
                                       (loop [es (nth adj u)
                                              queue queue dist dist]
                                         (if (empty? es)
                                           [queue dist]
                                           (let [w (first es)]
                                             (if (= (nth dist w) -1)
                                               (recur (rest es)
                                                      (conj queue w)
                                                      (assoc! dist w (+ dd 6)))
                                               (recur (rest es)
                                                      queue dist)))))]
                                   (recur queue (inc head) dist))))]
                    [(+ p 3 (* m 2))
                     (clojure.string/join
                      " "
                      (map str
                           (for [i (range 1 (inc n)) :when (not= i src)]
                             (nth dist i))))]))))))))))

(defn solve
  "Solves all cases from the token vector and prints the answers."
  [nums]
  (let [t (nth nums 0)]
    (if (< t 0)
      (report-error "bad input: case count must not be negative")
      (if (zero? t)
        nil
        (let [lines (loop [tc 0 p 1 acc (transient [])]
                      (if (>= tc t)
                        (persistent! acc)
                        (let [one (bfs-line nums p)]
                          (if (nil? one)
                            nil
                            (recur (inc tc) (first one)
                                   (conj! acc (second one)))))))]
          (if (nil? lines)
            (report-error "bad input: case shape wrong or vertex out of range")
            (do (println (clojure.string/join "\n" lines)) nil)))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
