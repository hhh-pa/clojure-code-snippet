;; max-ones-square-rectangle: largest all-1 square and rectangle
;;
;; Square side by 3-neighbour DP; rectangle area by histogram
;; stacks per row. Emits the max square area then max rectangle area.
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
    (let [h (nth nums 0) w (nth nums 1)
          g (mapv (fn [r] (mapv (fn [c] (nth nums (+ 2 (* r w) c))) (range w))) (range h))
          dp (atom (mapv (fn [_] (mapv (fn [_] 0) (range w))) (range h)))
          ms (atom 0)]
      (doseq [r (range h) c (range w)]
        (when (pos? (nth (nth g r) c))
          (let [v (if (or (zero? r) (zero? c)) 1
                    (inc (min (nth (nth @dp (dec r)) c) (nth (nth @dp r) (dec c)) (nth (nth @dp (dec r)) (dec c)))))]
            (swap! dp assoc-in [r c] v)
            (swap! ms max v))))
      (let [hist (fn [hh]
                   (let [v (conj (vec hh) 0)]
                     (loop [i 0 st [] best 0]
                       (if (>= i (count v)) best
                         (let [[st2 best2] (loop [st st best best]
                                                     (if (and (seq st) (> (nth v (peek st)) (nth v i)))
                                                       (let [H (nth v (peek st)) stp (pop st)
                                                             Wd (if (empty? stp) i (- i (peek stp) 1))]
                                                         (recur stp (max best (* H Wd))))
                                                       [st best]))]
                           (recur (inc i) (conj st2 i) best2))))))
            he (atom (vec (repeat w 0)))
            mr (atom 0)]
        (doseq [r (range h)]
          (doseq [c (range w)]
            (swap! he assoc c (if (pos? (nth (nth g r) c)) (inc (nth @he c)) 0)))
          (swap! mr max (hist @he)))
        (println (* @ms @ms))
        (println @mr)))))
