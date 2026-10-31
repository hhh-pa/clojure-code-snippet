;; selection-sort: pure Clojure selection sort (Sedgewick Ch 2.1).
;;
;; Repeatedly selects the minimum element from the unsorted suffix
;; a[i..n-1] and swaps it into position i. O(n^2) comparisons and at
;; most n swaps on every input, O(1) extra space beyond the transient
;; working vector. Pure Clojure with no host interop. Reads n followed
;; by n signed 64-bit integers and prints the sorted integers
;; separated by single spaces on one line.

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
  "Parse tok as a decimal long integer."
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

(defn selection-sort
  "Selection sort of xs in O(n^2) time and O(1) extra space."
  [xs]
  (let [v (vec xs)
        n (count v)]
    (if (<= n 1)
      v
      (let [a (transient v)]
        (loop [i 0]
          (when (< i n)
            (let [min-idx (loop [j (inc i)
                                 best i
                                 best-val (nth a i)]
                            (if (< j n)
                              (let [vj (nth a j)]
                                (if (< vj best-val)
                                  (recur (inc j) j vj)
                                  (recur (inc j) best best-val)))
                              best))]
              (when (not= min-idx i)
                (let [tmp (nth a i)]
                  (assoc! a i (nth a min-idx))
                  (assoc! a min-idx tmp))))
            (recur (inc i))))
        (persistent! a)))))

(defn -main
  "Read n followed by n integers and print them sorted."
  [input]
  (let [toks (tokenize input)]
    (when (seq toks)
      (let [n (parse-token (first toks))]
        (cond
          (or (nil? n) (neg? n))
          (binding [*out* *err*]
            (println "error: first token must be a non-negative integer count"))
          (< (dec (count toks)) n)
          (binding [*out* *err*]
            (println "error: fewer elements than count n"))
          :else
          (let [vals (mapv parse-token (take n (rest toks)))]
            (if (some nil? vals)
              (binding [*out* *err*]
                (println "error: expected valid 64-bit integers"))
              (let [sorted (selection-sort vals)]
                (println (apply str (interpose " " (map str sorted))))))))))))

(-main (slurp *in*))
