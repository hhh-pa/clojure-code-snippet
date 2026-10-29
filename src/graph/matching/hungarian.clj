;; Hungarian assignment with dual potentials in pure clojure.
;; Each of the n rows runs a Dijkstra like scan over m columns in
;; O(n m) time, for O(n**2 m) time total and O(n m) space, the best
;; known practical bound for dense rectangular assignment. Update
;; order matches the old run exactly, so totals and row matches
;; print the same. All loops use recur with transient work tables.
;; Input shape: n m with 1 <= n <= m, then n rows of m costs.
;; Output is the min total plus one i match line per row. Blank
;; stdin prints nothing. Badly shaped input prints one message to
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

(defn hungarian
  "Min cost assignment of n rows to m columns with n <= m. cost is
   a flat row major table with stride m + 1 and 1 based rows and
   columns. Returns the pair of min total and row match vector."
  [cost n m]
  (let [stride (inc m)
        inf 1000000000000000000
        u (transient (vec (repeat (inc n) 0)))
        v (transient (vec (repeat (inc m) 0)))
        p (transient (vec (repeat (inc m) 0)))
        way (transient (vec (repeat (inc m) 0)))]
    (loop [i 1 u u v v p p way way]
      (if (> i n)
        [(-' (nth v 0))
         (loop [j 1 match (transient (vec (repeat (inc n) 0)))]
           (if (> j m)
             (persistent! match)
             (let [pj (nth p j)]
               (recur (inc j)
                      (if (<= pj n) (assoc! match pj j) match)))))]
        (let [p (assoc! p 0 i)
              minv (transient (vec (repeat (inc m) inf)))
              used (transient (vec (repeat (inc m) false)))
              [u v p way] (loop [j0 0 u u v v p p way way minv minv used used]
                            (let [used (assoc! used j0 true)
                                  i0 (nth p j0)
                                  [minv way delta j1]
                                  (loop [j 1 minv minv way way
                                         delta inf j1 0]
                                    (if (> j m)
                                      [minv way delta j1]
                                      (if (nth used j)
                                        (recur (inc j) minv way delta j1)
                                        (let [cur (-' (nth cost (+ (* i0 stride) j)) (nth u i0) (nth v j))
                                              [minv way]
                                              (if (< cur (nth minv j))
                                                [(assoc! minv j cur)
                                                 (assoc! way j j0)]
                                                [minv way])]
                                          (if (< (nth minv j) delta)
                                            (recur (inc j) minv way
                                                   (nth minv j) j)
                                            (recur (inc j) minv way
                                                   delta j1))))))
                                  [u v minv] (loop [j 0 u u v v minv minv]
                                               (if (> j m)
                                                 [u v minv]
                                                 (if (nth used j)
                                                   (recur (inc j)
                                                          (assoc! u (nth p j)
                                                                  (+' (nth u (nth p j)) delta))
                                                          (assoc! v j
                                                                  (-' (nth v j) delta))
                                                          minv)
                                                   (recur (inc j) u v
                                                          (assoc! minv j
                                                                  (-' (nth minv j) delta))))))
                                  j1 j1]
                              (if (not= (nth p j1) 0)
                                (recur j1 u v p way minv used)
                                [u v
                                 (loop [jj j1 p p]
                                   (if (zero? jj)
                                     p
                                     (let [pj (nth way jj)]
                                       (recur pj (assoc! p jj (nth p pj))))))
                                 way])))]
          (recur (inc i) u v p way))))))

(defn solve
  "Solves one instance from the token vector and prints the answer."
  [nums]
  (let [c (count nums)
        n (nth nums 0 nil)
        m (nth nums 1 nil)]
    (if (or (< c 2) (< n 0) (< m 1) (> n m) (< c (+' 2 (*' n m))))
      (report-error "bad input: want n m with 1 <= m and n <= m plus costs")
      (if (zero? n)
        (do (println 0) nil)
        (let [stride (inc m)
              cost (loop [i 1 t 2
                          cost (transient (vec (repeat (* (inc n) stride) 0)))]
                     (if (> i n)
                       (persistent! cost)
                       (recur (inc i) (+ t m)
                              (loop [j 1 cost cost]
                                (if (> j m)
                                  cost
                                  (recur (inc j)
                                         (assoc! cost (+ (* i stride) j)
                                                 (nth nums (+ t (dec j))))))))))
              [total match] (hungarian cost n m)
              lines (transient [(str total)])]
          (loop [i 1 lines lines]
            (if (> i n)
              (do (println (clojure.string/join "\n" (persistent! lines))) nil)
              (recur (inc i) (conj! lines (str i " " (nth match i)))))))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
