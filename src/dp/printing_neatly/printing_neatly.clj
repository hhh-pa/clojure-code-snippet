;; printing-neatly: Knuth-Plass line breaking
;;
;; Backward DP over word breaks: cubic penalty on trailing space
;; except the free last line. Emits the minimum raggedness.
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


(let [toks (tokenize (slurp *in*))]
  (when (>= (count toks) 2)
    (let [n (parse-token (nth toks 0)) M (parse-token (nth toks 1))
          words (subvec toks 2)
          lens (mapv count words)
          inf (bit-shift-left 1 60)
          cost (atom (assoc (vec (repeat (inc n) inf)) n 0))]
      (doseq [i (range (dec n) -1 -1)]
        (loop [j i ll 0]
          (when (< j n)
            (let [nl (+ ll (nth lens j) (if (> j i) 1 0))]
              (when (<= nl M)
                (let [gap (- M nl)
                      c (if (= j (dec n)) 0 (* gap gap gap))]
                  (swap! cost assoc i (min (nth @cost i) (+ c (nth @cost (inc j))))))
                (recur (inc j) nl))))))
      (println (nth @cost 0)))))
