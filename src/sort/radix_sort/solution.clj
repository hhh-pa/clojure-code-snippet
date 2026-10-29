;; radix-sort: pure Clojure least-significant-digit radix sort.
;;
;; Eight stable base-256 passes over the 64 bits order every value;
;; flipping the sign bit first maps signed order onto unsigned key
;; order, so the full long range sorts correctly. O(n) time and O(n)
;; space with a constant eight passes, keeping the sort stable.
;; Pure Clojure with no
;; host interop: persistent vectors of bucket vectors, loop and
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

(def radix-base
  "Buckets per pass: one per byte value."
  256)

(def radix-passes
  "Passes needed to cover all 64 bits one byte at a time."
  8)

(defn radix-sort
  "Least-significant-digit radix sort of xs. O(n) time in eight
   stable passes and O(n) space, keeping equal values in order."
  [xs]
  (let [v (vec xs)
        n (count v)]
    (if (<= n 1)
      v
      (let [flip (bit-shift-left 1 63)
            keyed (mapv (fn [x] (bit-xor x flip)) v)]
        (->> (loop [pass 0 vs keyed]
               (if (>= pass radix-passes)
                 vs
                 (let [shift (* pass 8)
                       buckets (reduce (fn [bs k]
                                         (let [d (bit-and (bit-shift-right k shift) 255)]
                                           (assoc bs d (conj (nth bs d) k))))
                                       (vec (repeat radix-base []))
                                       vs)]
                   (recur (inc pass) (into [] cat buckets)))))
             (mapv (fn [k] (bit-xor k flip))))))))

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
                 (radix-sort)
                 (format-sorted)
                 (print))))))))

(-main (slurp *in*))
