;; Dijkstra shortest reach over many cases in pure clojure. A
;; sorted set of [dist vertex] pairs acts as the heap with O(log V)
;; updates and stale entries skipped on the way out, giving O(E log
;; V) time and O(V + E) space per case, the best known practical
;; bound for non negative weights. Output keeps the old shape with
;; minus one for unreachable verts and one line per case.
;; Input shape: case count t, then per case n m, m lines x y r with
;; 1 based verts, and a source. Blank stdin prints nothing. Badly
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

(defn dijkstra-line
  "Runs one case from token position p and returns the pair of next
   position and output line, or nil when the shape is wrong."
  [nums p]
  (let [c (count nums)]
    (when (>= (- c p) 2)
      (let [n (nth nums p)
            m (nth nums (inc p))]
        (when (and (>= n 1) (>= m 0) (>= (- c p) (+' 3 (*' m 3))))
          (let [adj (loop [i 0 adj (transient (vec (repeat n [])))]
                      (if (>= i m)
                        (persistent! adj)
                        (let [x (nth nums (+ p 2 (* i 3)))
                              y (nth nums (+ p 3 (* i 3)))
                              r (nth nums (+ p 4 (* i 3)))]
                          (if (or (< x 1) (> x n) (< y 1) (> y n))
                            nil
                            (let [a (dec x) b (dec y)]
                              (recur (inc i)
                                     (-> adj
                                         (assoc! a (conj (nth adj a) [b r]))
                                         (assoc! b (conj (nth adj b)
                                                         [a r])))))))))]
            (when (some? adj)
              (let [src (nth nums (+ p 2 (* m 3)))]
                (when (and (>= src 1) (<= src n))
                  (let [s (dec src)
                        dist (assoc! (transient (vec (repeat n -1))) s 0)
                        dist (loop [pq (sorted-set [0 s]) dist dist]
                               (if (empty? pq)
                                 dist
                                 (let [[cd v] (first pq)
                                       pq (disj pq [cd v])]
                                   (if (= cd (nth dist v))
                                     (let [[pq dist]
                                           (loop [es (nth adj v)
                                                  pq pq dist dist]
                                             (if (empty? es)
                                               [pq dist]
                                               (let [[to w] (first es)
                                                     ndv (+' cd w)
                                                     old (nth dist to)]
                                                 (if (or (= old -1) (< ndv old))
                                                   (recur (rest es)
                                                          (conj pq [ndv to])
                                                          (assoc! dist to ndv))
                                                   (recur (rest es)
                                                          pq dist)))))]
                                       (recur pq dist))
                                     (recur pq dist)))))]
                    [(+ p 3 (* m 3))
                     (str (apply str (map (fn [i] (str (nth dist i) " "))
                                              (remove #(= % s) (range n))))
                          "\n")]))))))))))

(defn solve
  "Solves all cases from the token vector and prints the answers."
  [nums]
  (let [t (nth nums 0)]
    (if (< t 0)
      (report-error "bad input: case count must not be negative")
      (if (zero? t)
        nil
        (let [text (loop [tc 0 p 1 acc ""]
                     (if (>= tc t)
                       acc
                       (let [one (dijkstra-line nums p)]
                         (if (nil? one)
                           nil
                           (recur (inc tc) (first one)
                                  (str acc (second one)))))))]
          (if (nil? text)
            (report-error "bad input: case shape wrong or vertex out of range")
            (do (print text) nil)))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
