;; quickselect: pure Clojure randomized selection with median driver.
;;
;; Finds the element of rank k without sorting: pick a random pivot,
;; split the range into less, equal, and greater in one pass, and
;; continue in the part holding rank k. O(n) expected time on every
;; input, O(n) on inputs with many duplicates thanks to the
;; three-way split, O(1) extra space on a transient working vector.
;; Pure Clojure with no host interop: one transient vector, loop and
;; recur. The driver reads n followed by values from stdin and
;; prints the element of rank (quot n 2).

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

(defn quickselect
  "Element of rank k (zero based) from xs. O(n) expected time, O(1)
   extra space. A random pivot plus a three-way split keeps every
   input fast, including inputs with heavy duplicates."
  [xs k]
  (let [v (vec xs)
        n (count v)]
    (if (<= n 1)
      (first v)
      (let [a (transient v)]
        (loop [lo 0 hi (dec n)]
          (if (>= lo hi)
            (nth a k)
            (let [pivot (nth a (+ lo (rand-int (inc (- hi lo)))))
                  [lt gt] (loop [lt lo i lo gt hi]
                             (if (> i gt)
                               [lt gt]
                               (let [x (nth a i)]
                                 (cond
                                   (< x pivot)
                                   (let [t (nth a lt)]
                                     (assoc! a lt x)
                                     (assoc! a i t)
                                     (recur (inc lt) (inc i) gt))
                                   (> x pivot)
                                   (let [t (nth a gt)]
                                     (assoc! a gt x)
                                     (assoc! a i t)
                                     (recur lt i (dec gt)))
                                   :else
                                   (recur lt (inc i) gt)))))]
              (cond
                (< k lt) (recur lo (dec lt))
                (> k gt) (recur (inc gt) hi)
                :else pivot))))))))

(defn -main
  "Read n followed by values from input and print the element of
   rank (quot n 2). Only the first n values are used and further
   tokens are ignored, matching the original driver. Blank input
   prints nothing. Malformed or inconsistent input prints a message
   to stderr and returns normally."
  [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [n (parse-token (first toks))]
        (cond
          (nil? n)
          (binding [*out* *err*]
            (println "error: first token must be the element count"))
          (neg? n)
          (binding [*out* *err*]
            (println "error: element count must not be negative"))
          (zero? n)
          (binding [*out* *err*]
            (println "error: no elements to choose the median from"))
          (< (dec (count toks)) n)
          (binding [*out* *err*]
            (println "error: count says n elements but fewer values follow"))
          :else
          (let [vals (mapv parse-token (take n (rest toks)))]
            (if (some nil? vals)
              (binding [*out* *err*]
                (println "error: expected integers separated by whitespace"))
              (println (quickselect vals (quot n 2))))))))))

(-main (slurp *in*))
