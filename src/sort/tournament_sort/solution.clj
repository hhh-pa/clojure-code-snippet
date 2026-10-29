;; tournament-sort: pure Clojure tournament sort.
;;
;; A winner tree over the input holds each pairwise minimum at its
;; parent; the root is the global minimum. Extracting the root and
;; replaying its path to the leaf yields the next minimum, so n
;; extractions sort the input. O(n log n) time and O(n) space. Not
;; stable. Empty slots compare larger than every value by
;; construction, never through a sentinel value. Pure Clojure with
;; no host interop: transient vectors, loop and recur. Input and
;; output match the shared sort driver contract: stdin holds n
;; followed by values, stdout is [v1,v2,...].

(def digit-values
  "Digit character to numeric value."
  (zipmap "0123456789" (range)))

(def separators
  "Whitespace characters that separate input tokens."
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize
  "Split s on whitespace into a vector of token strings."
  [s]
  (let [done (reduce (fn [{:keys [tokens current]} ch]
                       (if (contains? separators ch)
                         (if (empty? current)
                           {:tokens tokens :current []}
                           {:tokens (conj tokens (apply str current))
                            :current []})
                         {:tokens tokens :current (conj current ch)}))
                     {:tokens [] :current []}
                     s)]
    (if (empty? (:current done))
      (:tokens done)
      (conj (:tokens done) (apply str (:current done))))))

(def min-long
  "Most negative long value: 1 shifted into the sign bit."
  (bit-shift-left 1 63))

(def max-long
  "Largest long value: every bit below the sign bit set."
  (bit-not min-long))

(defn parse-token
  "Parse tok as a decimal long integer. An optional leading minus or
   plus sign is accepted. Returns nil when tok is malformed or falls
   outside the long range; the bounds check runs before each
   multiply and add step so no step can overflow."
  [tok]
  (let [chs (seq tok)]
    (when (seq chs)
      (let [head (first chs)
            neg? (= head \-)
            rest-chs (if (or neg? (= head \+)) (rest chs) chs)]
        (when (seq rest-chs)
          (if neg?
            (reduce (fn [acc ch]
                      (when (some? acc)
                        (let [d (get digit-values ch)]
                          (when (and (some? d)
                                     (>= acc (quot (+ min-long d) 10)))
                            (- (* acc 10) d)))))
                    0 rest-chs)
            (reduce (fn [acc ch]
                      (when (some? acc)
                        (let [d (get digit-values ch)]
                          (when (and (some? d)
                                     (<= acc (quot (- max-long d) 10)))
                            (+ (* acc 10) d)))))
                    0 rest-chs)))))))

(defn format-sorted
  "Render xs as [v1,v2,...] with no spaces or trailing newline."
  [xs]
  (str "[" (apply str (interpose "," (map str xs))) "]"))

(defn tournament-sort
  "Tournament sort of xs. O(n log n) time and O(n) space: a linear
   winner-tree build plus n logarithmic replays, one per extracted
   minimum. Empty leaves lose every match, so extreme values sort
   correctly."
  [xs]
  (let [v (vec xs)
        n (count v)]
    (if (<= n 1)
      v
      (let [size (loop [s 1] (if (< s n) (recur (* s 2)) s))
            tree (transient (vec (repeat (* 2 size) -1)))
            winner (fn [x y]
                     (cond (neg? x) y
                           (neg? y) x
                           (<= (nth v x) (nth v y)) x
                           :else y))]
        (loop [i 0]
          (when (< i size)
            (assoc! tree (+ size i) (if (< i n) i -1))
            (recur (inc i))))
        (loop [i (dec size)]
          (when (>= i 1)
            (assoc! tree i (winner (nth tree (* 2 i))
                                   (nth tree (inc (* 2 i)))))
            (recur (dec i))))
        (let [out (transient (vec (repeat n nil)))]
          (loop [k 0]
            (when (< k n)
              (let [w (nth tree 1)]
                (assoc! out k (nth v w))
                (assoc! tree (+ size w) -1)
                (loop [i (quot (+ size w) 2)]
                  (when (>= i 1)
                    (assoc! tree i (winner (nth tree (* 2 i))
                                           (nth tree (inc (* 2 i)))))
                    (recur (quot i 2))))
                (recur (inc k)))))
          (persistent! out))))))

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
            (->> (rest nums)
                 (take k)
                 (vec)
                 (tournament-sort)
                 (format-sorted)
                 (print))))))))

(-main (slurp *in*))
