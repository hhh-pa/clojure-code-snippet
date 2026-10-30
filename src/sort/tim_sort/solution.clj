;; insertion-sort: pure Clojure insertion sort.
;;
;; Grow the sorted prefix one value at a time, shifting larger values
;; right to open each slot. O(n squared) worst and average time, O(n)
;; best time on sorted input, O(1) extra space beyond the working
;; vector, which a transient keeps updateable in place. Stable and
;; adaptive. The quadratic bound is inherent to insertion sort and
;; matches its role as the measured quadratic pilot. Pure Clojure
;; with no host interop: one transient vector, loop and recur.
;; Input and output match the shared sort driver contract: stdin
;; holds n followed by values, stdout is [v1,v2,...].

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

(defn read-int [tok] (parse-token tok))

(defn format-sorted
  "Render xs as [v1,v2,...] with no spaces or trailing newline."
  [xs]
  (str "[" (apply str (interpose "," (map str xs))) "]"))


(defn insertion-run [a lo hi]
  (loop [a (vec a) i (inc lo)]
    (if (>= i hi) a
      (let [cur (nth a i)
            res (loop [a a j (dec i)]
                  (if (and (>= j lo) (> (nth a j) cur))
                    (recur (assoc a (inc j) (nth a j)) (dec j)) [a j]))]
        (recur (assoc (first res) (inc (second res)) cur) (inc i))))))
(defn merge-runs [a lo mid hi]
  (let [left (subvec a lo mid) right (subvec a mid hi)]
    (loop [i 0 j 0 k lo acc (vec a)]
      (cond (and (< i (count left)) (< j (count right))) (if (<= (nth left i) (nth right j)) (recur (inc i) j (inc k) (assoc acc k (nth left i))) (recur i (inc j) (inc k) (assoc acc k (nth right j))))
            (< i (count left)) (recur (inc i) j (inc k) (assoc acc k (nth left i)))
            (< j (count right)) (recur i (inc j) (inc k) (assoc acc k (nth right j)))
            :else acc))))
(defn tim-sort [v]
  (let [n (count v) min-run 32]
    (if (< n 2) v
      (let [a0 (loop [a (vec v) lo 0] (if (>= lo n) a (recur (insertion-run a lo (min (+ lo min-run) n)) (+ lo min-run))))]
        (loop [a a0 sz min-run]
          (if (>= sz n) a
            (recur (loop [a a lo 0] (if (>= (+ lo sz) n) a (let [mid (+ lo sz) hi (min (+ lo (* 2 sz)) n)] (if (<= (nth a (dec mid)) (nth a mid)) (recur a (+ lo (* 2 sz))) (recur (merge-runs a lo mid hi) (+ lo (* 2 sz))))))) (* 2 sz))))))))
(defn -main []
  (let [input (slurp *in*) toks (tokenize input) n (if (seq toks) (read-int (first toks)) 0)
        vals (mapv read-int (take n (rest toks))) sorted (tim-sort vals)]
    (print (str "[" (clojure.string/join "," sorted) "]"))))
(-main)
