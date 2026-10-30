;; gale-shapley: stable marriage by men-proposing
;;
;; Men propose down their lists; women keep the best suitor seen.
;; Emits the man-optimal stable pairs as m w lines.
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
          mp (mapv (fn [i] (mapv (fn [j] (nth nums (+ 1 (* i n) j))) (range n))) (range n))
          wp (mapv (fn [i] (mapv (fn [j] (nth nums (+ 1 (* n n) (* i n) j))) (range n))) (range n))
          rank (mapv (fn [w] (reduce (fn [acc r] (assoc acc (nth (nth wp w) r) r)) (vec (repeat n 0)) (range n))) (range n))
          wife (atom (vec (repeat n -1)))
          husb (atom (vec (repeat n -1)))
          nxt (atom (vec (repeat n 0)))]
      (loop [free (vec (range n))]
        (when (seq free)
          (let [m (first free)
                w (nth (nth mp m) (nth @nxt m))]
            (swap! nxt assoc m (inc (nth @nxt m)))
            (cond (= (nth @husb w) -1)
                  (do (swap! wife assoc m w) (swap! husb assoc w m)
                      (recur (vec (rest free))))
                  (< (nth (nth rank w) m) (nth (nth rank w) (nth @husb w)))
                  (let [ex (nth @husb w)]
                    (swap! wife assoc ex -1)
                    (swap! wife assoc m w)
                    (swap! husb assoc w m)
                    (recur (vec (concat (rest free) [ex]))))
                  :else (recur (vec (concat (rest free) [m])))))))
      (doseq [m (range n)]
        (println (str m " " (nth @wife m)))))))
