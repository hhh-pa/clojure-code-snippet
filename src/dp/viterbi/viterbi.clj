;; viterbi: max log-probability path
;;
;; Viterbi over a topological pass: best[v] extends the best
;; predecessor by log edge probability. Emits the target score.
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
    (let [n (parse-token (nth toks 0)) m (parse-token (nth toks 1))
          adj (atom (mapv (fn [_] []) (range n)))]
      (doseq [i (range m)]
        (let [u (parse-token (nth toks (+ 2 (* i 3))))
              v (parse-token (nth toks (+ 3 (* i 3))))
              p (Double/parseDouble (nth toks (+ 4 (* i 3))))]
          (swap! adj update u conj [v p])))
      (let [s (parse-token (nth toks (+ 2 (* m 3))))
            t (parse-token (nth toks (+ 3 (* m 3))))
            neg -1e100
            best (atom (assoc (vec (repeat n neg)) s 0.0))]
        (doseq [u (range n)]
          (when (not= (nth @best u) neg)
            (doseq [[v p] (nth @adj u)]
              (when (pos? p)
                (let [c (+ (nth @best u) (Math/log p))]
                  (when (> c (nth @best v))
                    (swap! best assoc v c)))))))
        (let [ans (nth @best t)]
          (println (String/format java.util.Locale/US "%.6f"
                       (to-array [(double (if (= ans neg) -1.0 ans))]))))))))
