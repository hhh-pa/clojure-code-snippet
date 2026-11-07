;; insertion-sort: pure Clojure insertion sort.
;;
;; Grow the sorted prefix one value at a time, shifting larger values
;; right to open each slot. O(n squared) worst and average time, O(n)
;; best time on sorted input, O(1) extra space beyond the working
;; vector, which a transient keeps updateable in place. Stable and
;; adaptive. The quadratic bound is inherent to insertion sort and
;; matches its role as the measured quadratic pilot. Pure Clojure
;; with no host interop: one transient vector, loop and recur.
;; Input and output match the shared sort driver contract: stdin
;; holds n followed by values, stdout is [v1,v2,...].

(def digit-values
  "Digit character to numeric value."
  (zipmap "0123456789" (range)))

(def separators
  "Whitespace characters that separate input tokens."
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize
  "Split s on whitespace into a vector of token strings."
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

(def min-long
  "Most negative long value: 1 shifted into the sign bit."
  (bit-shift-left 1 63))

(def max-long
  "Largest long value: every bit below the sign bit set."
  (bit-not min-long))

(defn parse-token
  "Parse tok as a decimal long integer. An optional leading minus or
   plus sign is accepted. Returns nil when tok is malformed or falls
   outside the long range."
  [tok]
  (when (and (string? tok) (pos? (count tok)))
    (parse-long tok)))

(defn format-sorted
  "Render xs as [v1,v2,...] with no spaces or trailing newline."
  [xs]
  (str "[" (clojure.string/join "," xs) "]"))

(defn insertion-sort
  "Insertion sort of xs. O(n squared) worst time, O(n) best time on
   sorted input, O(1) extra space. Stable: equal values never move
   past each other."
  [xs]
  (let [v (vec xs)
        n (count v)]
    (if (<= n 1)
      v
      (let [a (transient v)]
        (loop [i 1]
          (when (< i n)
            (let [key (nth a i)
                  prev (nth a (dec i))]
              (when (> prev key)
                (assoc! a i prev)
                (loop [j (- i 2)]
                  (if (>= j 0)
                    (let [aj (nth a j)]
                      (if (> aj key)
                        (do (assoc! a (inc j) aj)
                            (recur (dec j)))
                        (assoc! a (inc j) key)))
                    (assoc! a 0 key)))))
            (recur (inc i))))
        (persistent! a)))))

(defn -main
  "Read n followed by values from input and print the sorted values
   as [v1,v2,...]. Only the first n values are used; extra tokens are
   ignored and missing values shorten the sort, matching the original
   driver. Blank input prints nothing. Malformed input prints a
   message to stderr and returns normally."
  [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [nums (mapv parse-token toks)]
        (if (some nil? nums)
          (binding [*out* *err*]
            (println "error: expected n followed by integers"))
          (let [avail (max 0 (dec (count nums)))
                k (min (max (first nums) 0) avail)]
            (-> (into [] (subvec nums 1 (inc k)))
                (insertion-sort)
                (format-sorted)
                (print))))))))

(-main (slurp *in*))
