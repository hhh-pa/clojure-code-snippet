;; Eulerian path or circuit with the Hierholzer method in pure
;; clojure. The walk itself costs O(E) time and O(E) space, which is
;; optimal. Each adjacency list is sorted once so the printed trail
;; always takes the smallest free edge first, adding an O(E log D)
;; sorting term that the old run also pays for the same fixed order.
;; Input shape: n m, then m directed edges u v with 0 based verts.
;; Output is NO EULERIAN PATH, or CIRCUIT or PATH plus the vertex
;; line. Blank stdin prints nothing. Badly shaped input prints one
;; message to stderr and returns with exit 0, since a nonzero exit
;; needs host interop, which this script avoids by design.

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

(defn hierholzer
  "Walks the Eulerian trail from start over adj, where each entry is
   a descending neighbor vector that pops the smallest edge first.
   Returns the trail in forward order. Costs O(E) time and O(E) space."
  [adj start]
  (loop [stack [start]
         adj adj
         path (transient [])]
    (if (empty? stack)
      (persistent! path)
      (let [u (peek stack)
            nbrs (nth adj u)]
        (if (empty? nbrs)
          (recur (pop stack) adj (conj! path u))
          (recur (conj stack (peek nbrs))
                 (assoc! adj u (pop nbrs))
                 path))))))

(defn solve
  "Solves one instance from the token vector and prints the answer."
  [nums]
  (let [c (count nums)
        n (nth nums 0 nil)
        m (nth nums 1 nil)]
    (if (or (< c 2) (< n 1) (< m 0) (< c (+' 2 (*' m 2))))
      (report-error "bad input: want n m with n at least 1 plus m edge pairs")
      (let [built (loop [i 0
                         adj (transient (vec (repeat n [])))
                         in-deg (transient (vec (repeat n 0)))
                         out-deg (transient (vec (repeat n 0)))]
                    (if (>= i m)
                      [adj in-deg out-deg]
                      (let [u (nth nums (+ 2 (* i 2)))
                            v (nth nums (+ 3 (* i 2)))]
                        (if (or (< u 0) (>= u n) (< v 0) (>= v n))
                          nil
                          (recur (inc i)
                                 (assoc! adj u (conj (nth adj u) v))
                                 (assoc! in-deg v (inc (nth in-deg v)))
                                 (assoc! out-deg u (inc (nth out-deg u))))))))]
        (if (nil? built)
          (report-error "bad input: edge vertex out of range")
          (let [[adj in-deg out-deg] built
                deg (loop [i 0 starts [] ends [] bad false]
                      (if (>= i n)
                        [starts ends bad]
                        (let [diff (- (nth out-deg i) (nth in-deg i))]
                          (cond
                            (= diff 1) (recur (inc i) (conj starts i) ends bad)
                            (= diff -1) (recur (inc i) starts (conj ends i) bad)
                            (not= diff 0) (recur (inc i) starts ends true)
                            :else (recur (inc i) starts ends bad)))))
                [starts ends bad] deg]
            (cond
              (or bad (> (count starts) 1) (> (count ends) 1)
                  (not= (count starts) (count ends)))
              (do (println "NO EULERIAN PATH") nil)
              :else
              (let [is-circuit (empty? starts)
                    start (if is-circuit
                            (loop [i 0]
                              (cond
                                (>= i n) 0
                                (> (nth out-deg i) 0) i
                                :else (recur (inc i))))
                            (first starts))
                    adj (loop [i 0 adj adj]
                          (if (>= i n)
                            adj
                            (recur (inc i)
                                   (assoc! adj i (vec (sort > (nth adj i)))))))
                    rev-path (hierholzer adj start)]
                (if (not= (count rev-path) (inc m))
                  (do (println "NO EULERIAN PATH") nil)
                  (do
                    (println (if is-circuit "CIRCUIT" "PATH"))
                    (println (clojure.string/join
                              " " (map str (rseq (vec rev-path)))))
                    nil))))))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
