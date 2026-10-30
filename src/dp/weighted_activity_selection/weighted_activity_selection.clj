;; weighted-activity-selection: interval scheduling DP by finish time
;;
;; Sorts activities by finish time, builds predecessors with binary
;; search, then M[j] = max(wj + M[p(j)], M[j-1]) with strict inclusion.
;; Emits the optimum weight and the sorted chosen indices.
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
          triples (mapv (fn [i] [(nth nums (+ 1 (* i 3))) (nth nums (+ 2 (* i 3))) (nth nums (+ 3 (* i 3)))]) (range n))
          o (vec (sort-by (fn [i] [(nth (nth triples i) 1) (nth (nth triples i) 0) i]) (range n)))
          pred (fn [j]
                 (loop [lo 0 hi (dec j) ans -1]
                   (if (> lo hi) ans
                     (let [m (quot (+ lo hi) 2)]
                       (if (<= (nth (nth triples (nth o m)) 1) (nth (nth triples (nth o j)) 0))
                         (recur (inc m) hi m)
                         (recur lo (dec m) ans))))))
          p (mapv pred (range n))
          m (loop [j 0 acc []]
              (if (>= j n) acc
                (let [incl (+ (nth (nth triples (nth o j)) 2) (if (>= (nth p j) 0) (nth acc (nth p j)) 0))
                      excl (if (> j 0) (nth acc (dec j)) 0)]
                  (recur (inc j) (conj acc (if (> incl excl) incl excl))))))
          sel (loop [j (dec n) acc []]
                (if (neg? j) acc
                  (let [incl (+ (nth (nth triples (nth o j)) 2) (if (>= (nth p j) 0) (nth m (nth p j)) 0))
                        excl (if (> j 0) (nth m (dec j)) 0)]
                    (if (> incl excl)
                      (recur (nth p j) (conj acc (nth o j)))
                      (recur (dec j) acc)))))]
      (println (if (zero? n) 0 (nth m (dec n))))
      (println (clojure.string/join " " (sort sel))))))
