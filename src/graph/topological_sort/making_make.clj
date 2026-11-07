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
  "Reads all signed integers from string s into a vector. Tokens
   split on chars at or below space; a token that is not an optional
   minus and ASCII digits within long range fails the whole scan with
   nil."
  [s]
  (if (nil? s)
    []
    (let [raw (clojure.string/split s #"[\x00-\x20]+")
          n (count raw)
          start (if (and (pos? n) (empty? (nth raw 0))) 1 0)]
      (loop [i start acc (transient [])]
        (if (>= i n)
          (persistent! acc)
          (let [t (nth raw i)
                v (when (not= (nth t 0) \+) (parse-long t))]
            (if (nil? v)
              nil
              (recur (inc i)
                     (conj! acc (if (= v -9223372036854775808) (bigint v) v))))))))))

(defn report-error
  "Prints message to stderr and returns nil so the driver ends calmly."
  [msg]
  (binding [*out* *err*]
    (println msg))
  nil)

(defn- heap-push!
  "Pushes v onto the binary max-heap in transient vector heap (1-indexed, size at 0)."
  [heap v]
  (let [n (inc (nth heap 0))]
    (assoc! heap 0 n)
    (loop [i n]
      (if (<= i 1)
        (assoc! heap 1 v)
        (let [p (bit-shift-right i 1)
              pv (nth heap p)]
          (if (< pv v)
            (do (assoc! heap i pv)
                (recur p))
            (assoc! heap i v)))))))

(defn- heap-pop!
  "Pops the maximum from transient max-heap."
  [heap]
  (let [n (nth heap 0)
        top (nth heap 1)
        last-v (nth heap n)
        n2 (dec n)]
    (assoc! heap 0 n2)
    (when (pos? n2)
      (loop [i 1]
        (let [l (bit-shift-left i 1)
              r (inc l)]
          (if (> l n2)
            (assoc! heap i last-v)
            (let [lv (nth heap l)
                  best-c (if (and (<= r n2) (> (nth heap r) lv)) r l)
                  cv (nth heap best-c)]
              (if (> cv last-v)
                (do (assoc! heap i cv)
                    (recur best-c))
                (assoc! heap i last-v)))))))
    top))

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
                head (transient (vec (repeat vv -1)))
                edge-to (transient (vec (repeat m 0)))
                edge-next (transient (vec (repeat m -1)))
                indeg (transient (vec (repeat vv 0)))
                ok? (loop [k (- m 1)]
                      (if (neg? k)
                        true
                        (let [from (nth nums (+ 2 (* k 2)))
                              to (nth nums (+ 3 (* k 2)))]
                          (if (or (< from 0) (>= from vv)
                                  (< to 0) (>= to vv))
                            false
                            (do (assoc! edge-to k to)
                                (assoc! edge-next k (nth head from))
                                (assoc! head from k)
                                (assoc! indeg to (inc (nth indeg to)))
                                (recur (dec k)))))))]
            (if-not ok?
              (report-error "bad input: edge vertex out of range")
              (let [heap (transient (vec (repeat (inc vv) 0)))]
                (loop [i 0]
                  (when (< i vv)
                    (when (zero? (nth indeg i))
                      (heap-push! heap i))
                    (recur (inc i))))
                (let [ans (transient [])]
                  (loop []
                    (when (pos? (nth heap 0))
                      (let [v (heap-pop! heap)]
                        (conj! ans v)
                        (loop [ei (nth head v)]
                          (when (not= ei -1)
                            (let [w (nth edge-to ei)
                                  ni (dec (nth indeg w))]
                              (assoc! indeg w ni)
                              (when (zero? ni)
                                (heap-push! heap w))
                              (recur (nth edge-next ei)))))
                        (recur))))
                  (let [res (persistent! ans)]
                    (if (empty? res)
                      (print "\n")
                      (print (str (clojure.string/join " " res) " \n"))))
                  nil)))))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
