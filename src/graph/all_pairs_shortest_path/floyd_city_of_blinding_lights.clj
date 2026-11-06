;; All pairs shortest paths with the Floyd Warshall method in pure clojure.
;; Time O(n**3) and space O(n**2), which is the best known practical
;; bound for dense all pairs shortest paths. The distance table is one
;; flat transient vector, so each update costs O(1) amortized time.
;; Input shape: n m, then m lines u v w with 1 based vertices, then a
;; query count q followed by q lines u v. Output holds one distance
;; per query line, with -1 for unreachable pairs. A later edge line
;; overwrites an earlier one for the same pair, matching the old run.
;; Blank stdin prints nothing. Badly shaped input prints one message
;; to stderr and returns normally with exit 0, since a nonzero exit
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

(defn floyd-warshall
  "Runs the triple loop over flat table d with row stride sd for
   vertices 1 through n. Entries at or above inf count as missing."
  [d sd n inf]
  (loop [k 1 d d]
    (if (> k n)
      d
      (recur (inc k)
             (loop [i 1 d d]
               (if (> i n)
                 d
                 (let [dik (nth d (+ (* i sd) k))]
                   (recur (inc i)
                          (if (< dik inf)
                            (loop [j 1 d d]
                              (if (> j n)
                                d
                                (let [idx (+ (* i sd) j)
                                      dkj (nth d (+ (* k sd) j))]
                                  (recur (inc j)
                                         (if (and (< dkj inf)
                                                  (< (+' dik dkj) (nth d idx)))
                                           (assoc! d idx (+' dik dkj))
                                           d)))))
                            d)))))))))

(defn solve
  "Solves one instance from the token vector and prints the answers."
  [nums]
  (let [c (count nums)
        n (nth nums 0 nil)
        m (nth nums 1 nil)]
    (if (or (< c 2) (< n 1) (< m 0) (< c (+' 2 (*' m 3))))
      (report-error "bad input: want n m with n at least 1 plus m edge triples")
      (let [stride (inc n)
            size (* stride stride)
            inf 1000000000
            d (transient (vec (repeat size inf)))
            d (loop [i 1 d d]
                (if (> i n) d (recur (inc i) (assoc! d (+ (* i stride) i) 0))))
            d (loop [k 0 d d]
                (if (>= k m)
                  d
                  (let [u (nth nums (+ 2 (* k 3)))
                        v (nth nums (+ 3 (* k 3)))
                        w (nth nums (+ 4 (* k 3)))]
                    (if (or (< u 1) (> u n) (< v 1) (> v n))
                      nil
                      (recur (inc k) (assoc! d (+ (* u stride) v) w))))))]
        (if (nil? d)
          (report-error "bad input: edge vertex out of range")
          (let [pos (+ 2 (* m 3))]
            (if (>= c (inc pos))
              (let [q (nth nums pos)]
                (if (or (< q 0) (< c (+' pos 1 (*' q 2))))
                  (report-error "bad input: want query count plus query pairs")
                  (let [qs (loop [k 0 acc []]
                             (if (>= k q)
                               acc
                               (let [u (nth nums (+ pos 1 (* k 2)))
                                     v (nth nums (+ pos 2 (* k 2)))]
                                 (if (or (< u 1) (> u n) (< v 1) (> v n))
                                   nil
                                   (recur (inc k) (conj acc [u v]))))))]
                    (if (nil? qs)
                      (report-error "bad input: query vertex out of range")
                      (let [d (floyd-warshall d stride n inf)
                            lines (map (fn [[u v]]
                                         (let [dst (nth d (+ (* u stride) v))]
                                           (str (if (>= dst inf) -1 dst))))
                                       qs)]
                        (when (seq lines)
                          (println (clojure.string/join "\n" lines)))
                        nil)))))
              (report-error "bad input: want query count plus query pairs"))))))))

(defn -main
  "Entry point used by the top level driver below."
  [s]
  (let [nums (parse-ints s)]
    (cond
      (nil? nums) (report-error "bad input: expected only integers and whitespace")
      (empty? nums) nil
      :else (solve nums))))

(-main (slurp *in*))
