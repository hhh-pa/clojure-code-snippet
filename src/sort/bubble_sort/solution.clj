;; bubble-sort: pure Clojure bubble sort with early-termination flag
;; and adjacent swap counter (CLRS Ch 2).
;;
;; Repeatedly sweeps adjacent pairs, swapping out-of-order elements
;; and counting total swaps; terminates early as soon as a pass makes
;; no swaps. O(n^2) worst and average time, O(n) best time on sorted
;; input, O(1) extra space beyond the transient working vector. Stable.
;; Pure Clojure with no host interop.

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

(defn bubble-sort
  "Bubble sort of xs with early termination; returns {:sorted v :swaps count}."
  [xs]
  (let [v (vec xs)
        n (count v)]
    (if (<= n 1)
      {:sorted v :swaps 0}
      (let [a (transient v)
            total-swaps
            (loop [i 0
                   swaps 0]
              (if (>= i (dec n))
                swaps
                (let [limit (- (dec n) i)
                      [pass-swaps swapped?]
                      (loop [j 0
                             s swaps
                             sw? false]
                        (if (>= j limit)
                          [s sw?]
                          (let [x (nth a j)
                                y (nth a (inc j))]
                            (if (> x y)
                              (do (assoc! a j y)
                                  (assoc! a (inc j) x)
                                  (recur (inc j) (inc s) true))
                              (recur (inc j) s sw?)))))]
                  (if swapped?
                    (recur (inc i) pass-swaps)
                    pass-swaps))))]
        {:sorted (persistent! a) :swaps total-swaps}))))

(defn -main
  "Read n followed by n integers; print sorted array on line 1 and swap count on line 2."
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
              (let [{:keys [sorted swaps]} (bubble-sort vals)]
                (println (apply str (interpose " " (map str sorted))))
                (println swaps)))))))))

(-main (slurp *in*))
