;; online-bipartite-matching: Karp-Vazirani-Vazirani RANKING
;;
;; Ranks offline vertices by a fixed-seed shuffle, then matches each
;; arriving online vertex to the free neighbour of smallest rank.
;; Emits the size and the pairs.
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
  (when (>= (count nums) 3)
    (let [nU (nth nums 0) nV (nth nums 1) m (nth nums 2)
          edges (mapv (fn [i] [(nth nums (+ 3 (* i 2))) (nth nums (+ 4 (* i 2)))]) (range m))
          adj (reduce (fn [acc [u v]] (update acc u conj v)) (mapv (fn [_] []) (range nU)) edges)
          x (atom 42)
          rnd (fn [n] (swap! x (fn [xx] (mod (+ (* 1103515245 xx) 12345) (bit-shift-left 1 31)))) (mod @x n))
          rk (atom (vec (range nV)))]
      (doseq [i (range (dec nV) 0 -1)]
        (let [j (rnd (inc i)) a (nth @rk i) b (nth @rk j)]
          (swap! rk assoc i b j a)))
      (let [pos (reduce (fn [acc r] (assoc acc (nth @rk r) r)) (vec (repeat nV 0)) (range nV))
            mate (atom (vec (repeat nU -1)))
            used (atom (vec (repeat nV false)))]
        (doseq [u (range nU)]
          (let [[best _] (reduce (fn [[b bp] v] (if (and (not (nth @used v)) (< (nth pos v) bp)) [v (nth pos v)] [b bp]))
                                 [-1 (inc nV)] (nth adj u))]
            (when (>= best 0)
              (swap! mate assoc u best)
              (swap! used assoc best true))))
        (let [prs (filterv (fn [u] (>= (nth @mate u) 0)) (range nU))]
          (println (count prs))
          (doseq [u prs] (println (str u " " (nth @mate u)))))))))
