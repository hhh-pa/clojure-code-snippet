;; intro-sort: pure Clojure introsort.
;;
;; Quicksort with a median-of-three pivot, an insertion sort cutoff
;; for tiny ranges, and a heapsort fallback once the recursion depth
;; passes twice the binary log of n. O(n log n) time worst case and
;; average, O(log n) stack. Not stable. Pure Clojure with no host
;; interop: one transient vector, loop and recur. Input and output
;; match the shared sort driver contract: stdin holds n followed by
;; values, stdout is [v1,v2,...].

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

(def intro-cutoff
  "Ranges below this size use insertion sort."
  16)

(defn intro-sort
  "Introsort of xs. O(n log n) time worst case: quicksort while the
   partitions stay balanced, heapsort once the depth budget runs
   out. O(log n) stack."
  [xs]
  (let [v (vec xs)
        n (count v)]
    (if (<= n 1)
      v
      (let [a (transient v)
            depth-limit (let [lg (loop [d 0 m n]
                                   (if (<= m 1) d (recur (inc d) (quot m 2))))]
                          (* 2 lg))]
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
                (sift [lo heap-n root]
                  (loop [r root]
                    (let [l (inc (* 2 r))]
                      (when (< l heap-n)
                        (let [r2 (inc l)
                              c (if (and (< r2 heap-n)
                                         (> (nth a (+ lo r2)) (nth a (+ lo l))))
                                  r2 l)]
                          (when (> (nth a (+ lo c)) (nth a (+ lo r)))
                            (swap (+ lo r) (+ lo c))
                            (recur c)))))))
                (heap-range [lo hi]
                  (let [m (inc (- hi lo))]
                    (loop [i (quot (- m 2) 2)]
                      (when (>= i 0)
                        (sift lo m i)
                        (recur (dec i))))
                    (loop [e (dec m)]
                      (when (pos? e)
                        (swap lo (+ lo e))
                        (sift lo e 0)
                        (recur (dec e))))))
                (part [lo hi]
                  (let [mid (quot (+ lo hi) 2)]
                    (when (> (nth a lo) (nth a mid)) (swap lo mid))
                    (when (> (nth a lo) (nth a hi)) (swap lo hi))
                    (when (> (nth a mid) (nth a hi)) (swap mid hi))
                    (swap mid hi)
                    (let [pivot (nth a hi)]
                      (loop [i lo j lo]
                        (if (< j hi)
                          (if (<= (nth a j) pivot)
                            (do (swap i j) (recur (inc i) (inc j)))
                            (recur i (inc j)))
                          (do (swap i hi) i))))))
                (isort [lo hi depth]
                  (loop [lo lo hi hi depth depth]
                    (let [size (inc (- hi lo))]
                      (cond
                        (<= size 1) nil
                        (< size intro-cutoff) (insertion lo hi)
                        (zero? depth) (heap-range lo hi)
                        :else (let [p (part lo hi)]
                                (isort lo (dec p) (dec depth))
                                (recur (inc p) hi (dec depth)))))))]
          (isort 0 (dec n) depth-limit)
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
                 (intro-sort)
                 (format-sorted)
                 (print))))))))

(-main (slurp *in*))
