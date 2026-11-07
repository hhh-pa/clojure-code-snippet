;; Kruskal minimum spanning forest in pure clojure. Sorting the
;; edges costs O(E log E) time, then union find with path
;; compression and union by size links them in O(E alpha V) time,
;; for O(E log E) time total and O(E) space, the best known
;; practical bound for this problem. The total prints the same for
;; every tie order, since all spanning trees share one weight.
;; Input shape: all integers on stdin as g-nodes g-edges followed
;; by triples u v w with 1 based verts, extra numbers ignored.
;; Output is the forest weight. Blank stdin prints nothing. Badly
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

(defn dsu-find!
  "Finds the root of x with path compression in transient parent."
  [parent x]
  (let [root (loop [u x]
               (let [p (nth parent u)]
                 (if (= p u) u (recur p))))]
    (loop [u x]
      (let [p (nth parent u)]
        (when (not= p root)
          (assoc! parent u root)
          (recur p))))
    root))

(defn solve
  "Solves one instance from the token vector and prints the answer."
  [nums]
  (let [c (count nums)]
    (if (< c 2)
      (report-error "bad input: want at least node and edge counts")
      (let [g-nodes (nth nums 0)
            g-edges (nth nums 1)]
        (if (or (< g-nodes 0) (< g-edges 0))
          (report-error "bad input: counts must not be negative")
          (let [m (min g-edges (quot (- c 2) 3))
                valid? (loop [k 0]
                         (if (>= k m)
                           true
                           (let [base (+ 2 (* k 3))
                                 u (nth nums base)
                                 v (nth nums (inc base))]
                             (if (or (< u 1) (> u g-nodes)
                                     (< v 1) (> v g-nodes))
                               false
                               (recur (inc k))))))]
            (if-not valid?
              (report-error "bad input: edge vertex out of range")
              (let [packed-keys (loop [k 0 acc (transient [])]
                                  (if (>= k m)
                                    (persistent! acc)
                                    (let [w (nth nums (+ 4 (* k 3)))]
                                      (recur (inc k)
                                             (conj! acc (bit-or (bit-shift-left (+ w 1073741824) 32) k))))))
                    sorted-keys (vec (sort packed-keys))
                    parent (transient (vec (range (inc g-nodes))))
                    sizes (transient (vec (repeat (inc g-nodes) 1)))
                    total (loop [i 0 total 0]
                            (if (>= i m)
                              total
                              (let [pk (nth sorted-keys i)
                                    k (bit-and pk 4294967295)
                                    base (+ 2 (* k 3))
                                    x (nth nums base)
                                    y (nth nums (inc base))
                                    w (nth nums (+ base 2))
                                    rx (dsu-find! parent x)
                                    ry (dsu-find! parent y)]
                                (if (= rx ry)
                                  (recur (inc i) total)
                                  (let [sx (nth sizes rx)
                                        sy (nth sizes ry)]
                                    (if (>= sx sy)
                                      (do (assoc! parent ry rx)
                                          (assoc! sizes rx (+ sx sy))
                                          (recur (inc i) (+' total w)))
                                      (do (assoc! parent rx ry)
                                          (assoc! sizes ry (+ sx sy))
                                          (recur (inc i) (+' total w)))))))))]
                (println (str total))
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
