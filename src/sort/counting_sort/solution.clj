;; counting-sort: pure Clojure adaptive counting sort.
;;
;; Count occurrences of each value, then emit every value as many
;; times as it was counted. A dense count vector covers ranges near
;; the input size in O(n plus range) time; a sparse frequency map
;; covers huge ranges in O(n plus u log u) time for u distinct
;; values. Space is O(range) dense and O(u) sparse. Pure Clojure
;; with no host interop: persistent vectors and maps, loop and
;; recur, reduce where it fits. Input and output match the shared
;; sort driver contract: stdin holds n followed by values, stdout
;; is [v1,v2,...].

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

(def dense-factor
  "Ranges up to this multiple of n use the dense count vector.
   Any constant keeps the dense path linear; wider ranges take
   the sparse path instead of a hopelessly large vector."
  4)

(defn counting-sort
  "Counting sort of xs. Dense ranges take O(n plus range) time and
   space; sparse ranges take O(n plus u log u) time and O(u) space
   for u distinct values and keeps equal values in order. Negative
   values work through the
   minimum offset, and full-range input takes the sparse path."
  [xs]
  (let [v (vec xs)
        n (count v)]
    (if (<= n 1)
      v
      (let [mn (reduce min v)
            mx (reduce max v)
            r (- (bigint mx) (bigint mn))]
        (if (<= r (* dense-factor (max n 1)))
          (let [size (long (inc r))
                counts (reduce (fn [c x]
                                 (let [i (- x mn)]
                                   (assoc c i (inc (nth c i)))))
                               (vec (repeat size 0))
                               v)
                values (take size (iterate inc mn))]
            (into [] (mapcat (fn [x] (repeat (nth counts (- x mn)) x)))
                  values))
          (let [freqs (reduce (fn [m x] (assoc m x (inc (get m x 0))))
                              {}
                              v)]
            (into [] (mapcat (fn [x] (repeat (get freqs x) x)))
                  (sort (keys freqs)))))))))

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
                 (counting-sort)
                 (format-sorted)
                 (print))))))))

(-main (slurp *in*))
