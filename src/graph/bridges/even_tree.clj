;; Even tree edge cuts in pure clojure. One iterative depth first
;; search roots the tree at vertex 1 in O(n) time, then a reverse
;; pass sums subtree sizes and counts edges to even subtrees, also
;; O(n). Total time O(n) and space O(n), which is optimal since all
;; edges must be read. The old recursive search is replaced by an
;; explicit stack, so deep trees cannot overflow the call stack.
;; Input shape: n m, then m lines u v with 1 based vertices of a
;; tree. Output is the count of removable edges. Blank stdin prints
;; nothing. Badly shaped input prints one message to stderr and
;; returns with exit 0, since a nonzero exit needs host interop,
;; which this script avoids by design.

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

(defn count-cuts
  "Counts edges whose child side holds an even subtree. adj maps each
   vertex to its neighbor vector. Runs in O(n) time and O(n) space."
  [adj n]
  (let [parent (assoc! (transient (vec (repeat (inc n) 0))) 1 -1)
        order (loop [stack [1] parent parent order (transient [])]
                (if (empty? stack)
                  (persistent! order)
                  (let [u (peek stack)
                        stack (pop stack)
                        order (conj! order u)
                        pair (reduce (fn [[stack parent] v]
                                       (if (zero? (nth parent v))
                                         [(conj stack v) (assoc! parent v u)]
                                         [stack parent]))
                                     [stack parent]
                                     (nth adj u))]
                    (recur (first pair) (second pair) order))))]
    (loop [nodes (rseq order)
           sizes (transient (vec (repeat (inc n) 1)))
           cuts 0]
      (if (empty? nodes)
        cuts
        (let [u (first nodes)
              p (nth parent u)]
          (if (<= p 0)
            (recur (rest nodes) sizes cuts)
            (let [su (nth sizes u)]
              (recur (rest nodes)
                     (assoc! sizes p (+ (nth sizes p) su))
                     (if (even? su) (inc cuts) cuts)))))))))

(defn solve
  "Solves one instance from the token vector and prints the answer."
  [nums]
  (let [c (count nums)
        n (nth nums 0 nil)
        m (nth nums 1 nil)]
    (if (or (< c 2) (< n 1) (< m 0) (< c (+' 2 (*' m 2))))
      (report-error "bad input: want n m with n at least 1 plus m edge pairs")
      (let [adj (loop [i 0
                       adj (transient (vec (repeat (inc n) [])))]
                  (if (>= i m)
                    (persistent! adj)
                    (let [u (nth nums (+ 2 (* i 2)))
                          v (nth nums (+ 3 (* i 2)))]
                      (if (or (< u 1) (> u n) (< v 1) (> v n))
                        nil
                        (recur (inc i)
                               (-> adj
                                   (assoc! u (conj (nth adj u) v))
                                   (assoc! v (conj (nth adj v) u))))))))]
        (if (nil? adj)
          (report-error "bad input: edge vertex out of range")
          (do (println (count-cuts adj n)) nil))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
