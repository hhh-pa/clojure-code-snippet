;; bottleneck-spanning-tree: MBST bottleneck via Kruskal
;;
;; Kruskal MST: its heaviest edge is the minimum bottleneck over
;; all spanning trees. Emits the bottleneck then the MST weight.
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
  (when (>= (count nums) 2)
    (let [n (nth nums 0) m (nth nums 1)
          es (mapv (fn [i] [(nth nums (+ 2 (* i 3))) (nth nums (+ 3 (* i 3))) (nth nums (+ 4 (* i 3)))]) (range m))
          o (vec (sort-by (fn [i] [(nth (nth es i) 2) (nth (nth es i) 0) (nth (nth es i) 1)]) (range m)))
          p (atom (vec (range n))) r (atom (vec (repeat n 0)))
          find (fn [x] (loop [x x] (if (= (nth @p x) x) x (recur (nth @p x)))))
          uni (fn [a b]
                (let [ra (find a) rb (find b)]
                  (if (= ra rb) false
                    (let [[ra rb] (if (< (nth @r ra) (nth @r rb)) [rb ra] [ra rb])]
                      (swap! p assoc rb ra)
                      (when (= (nth @r ra) (nth @r rb)) (swap! r assoc ra (inc (nth @r ra))))
                      true))))
          mw (atom 0) bn (atom 0) cnt (atom 0)]
      (doseq [i o]
        (let [[u v w] (nth es i)]
          (when (uni u v)
            (swap! mw + w)
            (swap! bn max w)
            (swap! cnt inc))))
      (if (and (pos? n) (not= @cnt (dec n)))
        (println "DISCONNECTED")
        (do (println @bn) (println @mw))))))
