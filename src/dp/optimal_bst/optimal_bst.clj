;; optimal-bst: optimal binary search tree
;;
;; CLRS Optimal-BST: weight and expected-cost tables over
;; intervals, roots tried in order. Emits the optimum cost.
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


(defn parse-decimal
  "Parse tok as a decimal floating value with Double/parseDouble so the
   result is correctly rounded exactly like the C++ and Java drivers."
  [tok]
  (Double/parseDouble tok))

(defn read-doubles-after
  "Read stdin tokens, skipping the first k, as doubles."
  [k]
  (mapv parse-decimal (subvec (vec (tokenize (slurp *in*))) k)))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks)
    (let [n (parse-token (nth toks 0))
          p (vec (cons 0.0 (mapv parse-decimal (subvec toks 1 (inc n)))))
          q (mapv parse-decimal (subvec toks (inc n)))
          e (atom (mapv (fn [_] (mapv (fn [_] 0.0) (range (+ n 2)))) (range (+ n 2))))
          w (atom (mapv (fn [_] (mapv (fn [_] 0.0) (range (+ n 2)))) (range (+ n 2))))]
      (doseq [i (range 1 (+ n 2))]
        (swap! e assoc-in [i (dec i)] (nth q (dec i)))
        (swap! w assoc-in [i (dec i)] (nth q (dec i))))
      (doseq [l (range 1 (inc n)) i (range 1 (inc (inc (- n l))))]
        (let [j (+ i l -1)]
          (swap! w assoc-in [i j] (+ (+ (nth (nth @w i) (dec j)) (nth p j)) (nth q j)))
          (swap! e assoc-in [i j] 1e100)
          (doseq [r (range i (inc j))]
            (let [c (+ (+ (nth (nth @e i) (dec r)) (nth (nth @e (inc r)) j)) (nth (nth @w i) j))]
              (when (< c (nth (nth @e i) j))
                (swap! e assoc-in [i j] c))))))
      (println (String/format java.util.Locale/US "%.6f" (to-array [(double (nth (nth @e 1) n))]))))))
