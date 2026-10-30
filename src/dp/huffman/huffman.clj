;; huffman: optimal prefix code by greedy merging
;;
;; Merges the two lightest trees (ties by smallest leaf index) until
;; one tree remains. Emits the total weighted cost and each symbol code
;; length in input order.
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
          fr (mapv #(nth nums (inc %)) (range n))]
      (if (<= n 1)
        (do (println 0) (println 0))
        (let [F (atom fr) MS (atom (vec (range n)))
              L (atom (mapv (fn [_] -1) (range n))) R (atom (mapv (fn [_] -1) (range n)))
              key (fn [Fv MSv i] [(nth Fv i) (nth MSv i) i])
              pq (atom (vec (sort-by (fn [i] [(nth fr i) i i]) (range n))))]
          (loop []
            (when (> (count @pq) 1)
              (let [a (first @pq) b (second @pq)
                    rest-pq (vec (drop 2 @pq))
                    id (count @F)
                    nF (conj @F (+ (nth @F a) (nth @F b)))
                    nMS (conj @MS (min (nth @MS a) (nth @MS b)))]
                (reset! F nF) (reset! MS nMS)
                (swap! L conj a) (swap! R conj b)
                (reset! pq (vec (sort-by (fn [i] (key nF nMS i)) (conj rest-pq id))))
                (recur))))
          (let [root (first @pq)
                dd (atom (mapv (fn [_] 0) (range (count @F))))]
            (loop [st [root]]
              (if (empty? st)
                nil
                (let [u (peek st) rest-st (pop st)]
                  (if (>= (nth @L u) 0)
                    (do
                      (swap! dd assoc (nth @L u) (inc (nth @dd u)))
                      (swap! dd assoc (nth @R u) (inc (nth @dd u)))
                      (recur (conj rest-st (nth @L u) (nth @R u))))
                    (recur rest-st)))))
            (println (reduce + (map * fr (subvec @dd 0 n))))
            (println (clojure.string/join " " (subvec @dd 0 n)))))))))
