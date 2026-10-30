;; seam-carving: vertical seam by dynamic programming
;;
;; Dynamic program over rows: each cell extends the cheapest of its three
;; upper neighbours (ties go to the leftmost column). Emits the minimum
;; seam energy and the 0-based seam column per row.
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
          vals (subvec nums 2)
          grid (mapv (fn [r] (mapv (fn [c] (nth vals (+ (* r w) c))) (range w))) (range h))
          inf (bit-shift-left 1 60)
          dp (atom (mapv (fn [r] (mapv (fn [c] (if (zero? r) (nth (nth grid r) c) inf)) (range w))) (range h)))
          par (atom (mapv (fn [_] (mapv (fn [_] 0) (range w))) (range h)))]
      (doseq [r (range 1 h) c (range w)]
        (let [[best bi] (loop [pcs [(dec c) c (inc c)] best inf bi c]
                              (if (empty? pcs)
                                [best bi]
                                (let [pc (first pcs)]
                                  (if (and (>= pc 0) (< pc w) (< (nth (nth @dp (dec r)) pc) best))
                                    (recur (rest pcs) (nth (nth @dp (dec r)) pc) pc)
                                    (recur (rest pcs) best bi)))))]
          (swap! dp assoc-in [r c] (+ (nth (nth grid r) c) best))
          (swap! par assoc-in [r c] bi)))
      (let [mc (loop [c 1 best 0] (if (>= c w) best (if (< (nth (nth @dp (dec h)) c) (nth (nth @dp (dec h)) best)) (recur (inc c) c) (recur (inc c) best))))
            seam (loop [r (dec h) c mc acc ()] (if (neg? r) acc (recur (dec r) (if (zero? r) 0 (nth (nth @par r) c)) (conj acc c))))]
        (println (nth (nth @dp (dec h)) mc))
        (println (clojure.string/join " " seam))))))
