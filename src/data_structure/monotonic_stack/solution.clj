(require 'clojure.string)

;; Monotonic Stack for Largest Rectangle in Histogram and Stock Span (Karumanchi Ch 4).
;; Optimal bounds: O(N) time and O(N) space; each bar index is pushed and popped
;; at most once in each monotonic stack pass.

(def digit-values
  "Map from each digit char to its numeric value."
  (zipmap "0123456789" (range)))

(def max-long
  "Largest signed 64-bit value."
  9223372036854775807)

(def min-long
  "Smallest signed 64-bit value."
  -9223372036854775808)

(defn fit-long
  "Returns n when it fits in a signed 64-bit long, else nil."
  [n]
  (when (<= min-long n max-long) n))

(def whitespace-chars
  "Chars treated as token separators on input."
  #{\space \tab \newline \return})

(defn tokenize
  "Split s into whitespace separated tokens in O(n) time."
  [s]
  (loop [cs (seq s) cur [] toks []]
    (if (nil? cs)
      (if (seq cur) (conj toks (apply str cur)) toks)
      (let [ch (first cs)]
        (if (contains? whitespace-chars ch)
          (recur (next cs) [] (if (seq cur) (conj toks (apply str cur)) toks))
          (recur (next cs) (conj cur ch) toks))))))

(defn parse-num
  "Parse tok as an integer, or nil when it is not one."
  [tok]
  (when (and (string? tok) (seq tok))
    (let [cs (seq tok)
          neg? (= (first cs) \-)
          ds (if neg? (next cs) cs)]
      (when (and (seq ds) (every? digit-values ds))
        (let [v (reduce (fn [acc ch] (+' (*' acc 10) (digit-values ch))) 0 ds)]
          (fit-long (if neg? (-' v) v)))))))

(defn fail
  "Report invalid input on stderr and return normally."
  [msg]
  (binding [*out* *err*]
    (println msg)))

(defn largest-rectangle
  "Compute the largest rectangle area in histogram h in O(n) time."
  [h n]
  (loop [i 0 st [] best 0]
    (if (> i n)
      best
      (let [cur-h (if (= i n) -1 (nth h i))
            [st2 best2]
            (loop [s st b best]
              (if (and (seq s) (>= (nth h (peek s)) cur-h))
                (let [top (peek s)
                      s-pop (pop s)
                      width (if (seq s-pop) (- i (peek s-pop) 1) i)
                      area (* (nth h top) width)]
                  (recur s-pop (if (> area b) area b)))
                [s b]))]
        (recur (inc i) (conj st2 i) best2)))))

(defn stock-spans
  "Compute the stock span array for heights h in O(n) time."
  [h n]
  (loop [i 0 st [] spans []]
    (if (>= i n)
      spans
      (let [cur-h (nth h i)
            st2 (loop [s st]
                  (if (and (seq s) (<= (nth h (peek s)) cur-h))
                    (recur (pop s))
                    s))
            sp (if (seq st2) (- i (peek st2)) (inc i))]
        (recur (inc i) (conj st2 i) (conj spans sp))))))

(defn parse-heights
  "Parse n non-negative heights from toks starting at index 1."
  [toks n]
  (if (< (count toks) (inc n))
    nil
    (loop [i 0 acc []]
      (if (>= i n)
        acc
        (let [v (parse-num (nth toks (inc i)))]
          (if (or (nil? v) (neg? v))
            nil
            (recur (inc i) (conj acc v))))))))

(defn -main
  "Entry point for largest rectangle in histogram and stock span."
  [input]
  (let [toks (tokenize input)
        n (parse-num (first toks))]
    (if (or (nil? n) (neg? n))
      (fail "invalid input: expected non-negative count n")
      (let [h (parse-heights toks n)]
        (if (nil? h)
          (fail "invalid input: expected n non-negative integer heights")
          (let [area (largest-rectangle h n)
                spans (stock-spans h n)]
            (println (str area))
            (println (clojure.string/join " " (map str spans)))))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
