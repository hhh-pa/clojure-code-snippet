;; shell-sort: pure Clojure shell sort with Ciura gaps.
;;
;; Gapped insertion passes with the Ciura gap sequence move values
;; across the input in a few long jumps before the final plain
;; insertion pass tidies up. Near linearithmic in practice, far
;; better than the quadratic worst case of halving gaps, and O(1)
;; extra space beyond the working vector, which a transient keeps
;; updateable in place. Not stable. Pure Clojure with no host
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

(def ciura-gaps
  "Ciura gap sequence in decreasing order."
  [701 301 132 57 23 10 4 1])

(defn shell-gaps
  "Ciura gaps below n in decreasing order, extended by a factor of
   about 2.25 while larger gaps are still needed."
  [n]
  (let [gaps (loop [gs ciura-gaps]
               (if (>= (first gs) n)
                 gs
                 (recur (into [(quot (* (first gs) 9) 4)] gs))))]
    (filter (fn [g] (< g n)) gaps)))

(defn shell-sort
  "Shell sort of xs with Ciura gaps. Near linearithmic in practice,
   O(1) extra space."
  [xs]
  (let [v (vec xs)
        n (count v)]
    (if (<= n 1)
      v
      (let [a (transient v)]
        (loop [gaps (shell-gaps n)]
          (when (seq gaps)
            (let [gap (first gaps)]
              (loop [i gap]
                (when (< i n)
                  (let [tmp (nth a i)]
                    (loop [j i]
                      (if (and (>= j gap) (> (nth a (- j gap)) tmp))
                        (do (assoc! a j (nth a (- j gap)))
                            (recur (- j gap)))
                        (assoc! a j tmp))))
                  (recur (inc i)))))
            (recur (rest gaps))))
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
            (->> (rest nums)
                 (take k)
                 (vec)
                 (shell-sort)
                 (format-sorted)
                 (print))))))))

(-main (slurp *in*))
