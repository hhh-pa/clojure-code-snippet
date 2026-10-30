;; greedy-activity-selector: earliest-finish greedy selection
;;
;; Sorts by finish time and greedily keeps each activity starting no
;; earlier than the last kept finish. Emits the count and the chosen
;; 0-based indices in selection order.
;; Reads stdin tokens, writes the answer to stdout.

(def digit-values
  "Digit character to numeric value."
  (zipmap "0123456789" (range)))

(def separators
  "Whitespace characters that separate input tokens."
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize
  "Split s on whitespace into a vector of token strings."
  [s]
  (let [done (reduce (fn [acc ch]
                       (if (contains? separators ch)
                         (if (empty? (:current acc))
                           acc
                           {:tokens (conj (:tokens acc) (apply str (:current acc)))
                             :current []})
                         (update acc :current conj ch)))
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
  "Parse tok as a decimal long integer with optional sign. Returns
   nil when tok is malformed or out of long range; the bounds check
   runs before each multiply-and-add step so no step can overflow."
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

(defn read-longs
  "Read all of stdin as a vector of longs."
  []
  (mapv parse-token (tokenize (slurp *in*))))


(let [nums (read-longs)]
  (if (empty? nums)
    (do (println 0) (println))
    (let [n (nth nums 0)
          acts (mapv (fn [i] [(nth nums (+ 1 (* i 2))) (nth nums (+ 2 (* i 2)))]) (range n))
          o (vec (sort-by (fn [i] [(nth (nth acts i) 1) (nth (nth acts i) 0) i]) (range n)))
          sel (loop [js o last 0 first? true acc []]
                (if (empty? js) acc
                  (let [j (first js)]
                    (if (or first? (>= (nth (nth acts j) 0) last))
                      (recur (rest js) (nth (nth acts j) 1) false (conj acc j))
                      (recur (rest js) last first? acc)))))]
      (println (count sel))
      (println (clojure.string/join " " sel)))))
