;; prefix-scan: Blelloch up/down-sweep scan
;;
;; Padded Blelloch scan: up-sweep builds partial sums, down-sweep
;; distributes them into the exclusive scan. Emits exclusive then
;; inclusive scans.
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
  (when (>= (count nums) 1)
    (let [n (nth nums 0)
          a (mapv #(nth nums (inc %)) (range n))
          N (loop [x 1] (if (>= x (max n 1)) x (recur (* x 2))))
          t (atom (mapv #(if (< % n) (nth a %) 0) (range N)))]
      (loop [d 1]
        (when (< d N)
          (doseq [k (range 0 N (* 2 d))]
            (swap! t assoc (+ k (* 2 d) -1) (+ (nth @t (+ k (* 2 d) -1)) (nth @t (+ k d -1)))))
          (recur (* d 2))))
      (swap! t assoc (dec N) 0)
      (loop [d (quot N 2)]
        (when (>= d 1)
          (doseq [k (range 0 N (* 2 d))]
            (let [tmp (nth @t (+ k d -1))]
              (swap! t assoc (+ k d -1) (nth @t (+ k (* 2 d) -1)))
              (swap! t assoc (+ k (* 2 d) -1) (+ (nth @t (+ k (* 2 d) -1)) tmp))))
          (recur (quot d 2))))
      (println (clojure.string/join " " (subvec @t 0 n)))
      (println (clojure.string/join " " (map + (subvec @t 0 n) a))))))
