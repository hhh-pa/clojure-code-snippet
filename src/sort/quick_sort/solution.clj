;; quick-sort: pure Clojure randomized three-way quicksort.
;;
;; A random pivot avoids the quadratic worst case of a fixed pivot
;; on sorted input; the three-way split sorts ranges with many
;; duplicates in linear time; tiny ranges defer to one final
;; insertion pass; the smaller side recurses first so the stack
;; stays logarithmic. O(n log n) expected time on every input, O(n)
;; on equal keys, O(log n) stack, O(1) extra space beyond the
;; working vector. Not stable. Pure Clojure with no host interop:
;; one transient vector, loop and recur. Input and output match the
;; shared sort driver contract: stdin holds n followed by values,
;; stdout is [v1,v2,...].

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

(def quick-cutoff
  "Ranges below this size skip partitioning; one final insertion
   pass over the nearly sorted whole finishes them in linear time."
  16)

(defn quick-sort
  "Randomized three-way quicksort of xs. O(n log n) expected time
   on every input, O(n) on equal keys, O(log n) stack."
  [xs]
  (let [v (vec xs)
        n (count v)]
    (if (<= n 1)
      v
      (let [a (transient v)]
        (letfn [(swap [i j]
                  (let [t (nth a i)]
                    (assoc! a i (nth a j))
                    (assoc! a j t)))
                (insertion [lo hi]
                  (loop [i (inc lo)]
                    (when (<= i hi)
                      (let [key (nth a i)]
                        (loop [j (dec i)]
                          (if (and (>= j lo) (> (nth a j) key))
                            (do (assoc! a (inc j) (nth a j))
                                (recur (dec j)))
                            (assoc! a (inc j) key))))
                      (recur (inc i)))))
                (qsort [lo hi]
                  (loop [lo lo hi hi]
                    (let [size (inc (- hi lo))]
                      (when (>= size quick-cutoff)
                        (let [pivot (nth a (+ lo (rand-int size)))
                              [lt gt] (loop [lt lo i lo gt hi]
                                         (if (> i gt)
                                           [lt gt]
                                           (let [x (nth a i)]
                                             (cond
                                               (< x pivot)
                                               (do (swap lt i)
                                                   (recur (inc lt) (inc i) gt))
                                               (> x pivot)
                                               (do (swap i gt)
                                                   (recur lt i (dec gt)))
                                               :else
                                               (recur lt (inc i) gt)))))]
                          (if (< (- lt lo) (- hi gt))
                            (do (qsort lo (dec lt))
                                (recur (inc gt) hi))
                            (do (qsort (inc gt) hi)
                                (recur lo (dec lt)))))))))]
          (qsort 0 (dec n))
          (insertion 0 (dec n))
          (persistent! a))))))

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
                 (quick-sort)
                 (format-sorted)
                 (print))))))))

(-main (slurp *in*))
