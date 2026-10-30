;; deadline-scheduling: unit tasks with deadlines and penalties
;;
;; Greedy matroid scheduling: jobs in decreasing penalty order take
;; the latest free slot at or before their deadline. Emits the minimum
;; total penalty and the sorted scheduled indices.
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
          jobs (mapv (fn [i] [(nth nums (+ 1 (* i 2))) (nth nums (+ 2 (* i 2)))]) (range n))
          mx (reduce max 0 (mapv first jobs))
          o (vec (sort-by (fn [i] [(- (nth (nth jobs i) 1)) i]) (range n)))
          slot (atom (mapv (fn [_] -1) (range (inc mx))))
          ins (atom (mapv (fn [_] false) (range n)))]
      (doseq [i o]
        (let [t (loop [t (min (nth (nth jobs i) 0) mx)]
                  (cond (<= t 0) 0
                        (= (nth @slot t) -1) t
                        :else (recur (dec t))))]
          (when (>= t 1)
            (swap! slot assoc t i)
            (swap! ins assoc i true))))
      (let [sel (filterv #(nth @ins %) (range n))
            pen (reduce + 0 (map #(nth (nth jobs %) 1) (remove #(nth @ins %) (range n))))]
        (println pen)
        (println (clojure.string/join " " sel))))))
