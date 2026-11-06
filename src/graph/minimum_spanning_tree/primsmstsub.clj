;; Prim minimum spanning tree in pure clojure. A sorted set of
;; [weight vertex] pairs acts as the heap with O(log V) updates,
;; giving O(E log V) time and O(E) space, the best known
;; practical bound for heap based Prim runs. Each vertex joins
;; cheapest link, and stale heap entries are skipped on the way
;; out. The total prints the same for every tie order.
;; Input shape: n m, then m lines u v w with 1 based verts, then a
;; start vertex. Output is the tree weight reached from start.
;; Blank stdin prints nothing. Badly shaped input prints one
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
  "Reads all signed integers from string s into a vector. Tokens
   split on chars at or below space; a token that is not an optional
   minus and ASCII digits within long range fails the whole scan with
   nil."
  [s]
  (if (nil? s)
    []
    (reduce (fn [acc t]
              (if-let [v (when (re-matches #"-?[0-9]+" t) (parse-long t))]
                (conj acc (if (= v -9223372036854775808) (bigint v) v))
                (reduced nil)))
            []
            (filterv seq (clojure.string/split s #"[\x00-\x20]+")))))

(defn report-error
  "Prints message to stderr and returns nil so the driver ends calmly."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn prim-total
  "Grows the tree from start over adj, where each entry holds [to
   w] pairs. Returns the tree weight. Costs O(E log V) time."
  [adj start n]
  (loop [pq (sorted-set [0 start])
         visited (transient (vec (repeat (inc n) false)))
         joined 0
         total 0]
    (if (or (= joined n) (empty? pq))
      total
      (let [[w u] (first pq)
            pq (disj pq [w u])]
        (if (nth visited u)
          (recur pq visited joined total)
          (let [visited (assoc! visited u true)
                pq (reduce (fn [pq [v ew]]
                             (if (nth visited v) pq (conj pq [ew v])))
                           pq
                           (nth adj u))]
            (recur pq visited (inc joined) (+' total w))))))))

(defn solve
  "Solves one instance from the token vector and prints the answer."
  [nums]
  (let [c (count nums)
        n (nth nums 0 nil)
        m (nth nums 1 nil)]
    (if (or (< c 2) (< n 1) (< m 0) (< c (+' 3 (*' m 3))))
      (report-error "bad input: want n m plus m edge triples and start")
      (let [adj (loop [i 0 adj (transient (vec (repeat (inc n) [])))]
                  (if (>= i m)
                    (persistent! adj)
                    (let [u (nth nums (+ 2 (* i 3)))
                          v (nth nums (+ 3 (* i 3)))
                          w (nth nums (+ 4 (* i 3)))]
                      (if (or (< u 1) (> u n) (< v 1) (> v n))
                        nil
                        (recur (inc i)
                               (-> adj
                                   (assoc! u (conj (nth adj u) [v w]))
                                   (assoc! v (conj (nth adj v) [u w]))))))))
            start (nth nums (+ 2 (* m 3)))]
        (if (nil? adj)
          (report-error "bad input: edge vertex out of range")
          (if (or (< start 1) (> start n))
            (report-error "bad input: start vertex out of range")
            (do (println (str (prim-total adj start n))) nil)))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
