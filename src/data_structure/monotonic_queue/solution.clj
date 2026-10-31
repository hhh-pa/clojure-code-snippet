(require 'clojure.string)

;; Monotonic Deque for O(N) Sliding Window Maximum and Minimum (Karumanchi Ch 4).
;; Optimal bounds: O(N) time and O(N) space; each index enters and leaves the
;; monotonic deques at most once.

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

(defn parse-array
  "Parse n integers from toks starting at index 2."
  [toks n]
  (if (< (count toks) (+ n 2))
    nil
    (loop [i 0 acc []]
      (if (>= i n)
        acc
        (let [v (parse-num (nth toks (+ i 2)))]
          (if (nil? v)
            nil
            (recur (inc i) (conj acc v))))))))

(defn sliding-extremes
  "Compute sliding window extrema using a persistent vector + head cursor deque."
  [a n k cmp]
  (loop [i 0 dq [] head 0 out []]
    (if (>= i n)
      out
      (let [head2 (loop [h head]
                    (if (and (< h (count dq)) (<= (nth dq h) (- i k)))
                      (recur (inc h))
                      h))
            dq2 (loop [d dq]
                  (if (and (> (count d) head2)
                           (cmp (nth a (peek d)) (nth a i)))
                    (recur (pop d))
                    d))
            dq3 (conj dq2 i)
            out2 (if (>= i (dec k))
                   (conj out (nth a (nth dq3 head2)))
                   out)]
        (recur (inc i) dq3 head2 out2)))))

(defn -main
  "Entry point for sliding window maximum and minimum."
  [input]
  (let [toks (tokenize input)]
    (if (< (count toks) 2)
      (fail "invalid input: expected N and K")
      (let [n (parse-num (nth toks 0))
            k (parse-num (nth toks 1))]
        (if (or (nil? n) (nil? k) (<= n 0) (<= k 0) (> k n))
          (fail "invalid input: require 1 <= K <= N")
          (let [a (parse-array toks n)]
            (if (nil? a)
              (fail "invalid input: expected N integer elements")
              (let [maxs (sliding-extremes a n k <=)
                    mins (sliding-extremes a n k >=)]
                (println (clojure.string/join " " (map str maxs)))
                (println (clojure.string/join " " (map str mins)))))))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
