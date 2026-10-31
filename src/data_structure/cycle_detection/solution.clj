(require 'clojure.string)

;; Floyd's Tortoise and Hare Cycle Detection in Functional Graph / Linked List (Karumanchi Ch 3).
;; Optimal bounds: O(mu + lambda) <= O(N) time and O(1) auxiliary pointer space
;; beyond the O(N) input representation.

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

(defn parse-next
  "Parse n next pointers in [-1, n-1] from toks starting at index 2."
  [toks n]
  (if (< (count toks) (+ n 2))
    nil
    (loop [i 0 acc []]
      (if (>= i n)
        acc
        (let [v (parse-num (nth toks (+ i 2)))]
          (if (or (nil? v) (< v -1) (>= v n))
            nil
            (recur (inc i) (conj acc v))))))))

(defn floyd-detect
  "Run Floyd's tortoise and hare from start node s on functional graph nxt."
  [nxt s]
  (let [meet (loop [t s h s]
               (if (or (= h -1) (= (nth nxt h) -1))
                 nil
                 (let [nh (nth nxt h)]
                   (if (= (nth nxt nh) -1)
                     nil
                     (let [t2 (nth nxt t)
                           h2 (nth nxt nh)]
                       (if (= t2 h2)
                         t2
                         (recur t2 h2)))))))]
    (if (nil? meet)
      (loop [cur s steps 0]
        (if (= cur -1)
          (str "NO CYCLE " steps)
          (recur (nth nxt cur) (inc steps))))
      (let [[entry mu] (loop [t s h meet m 0]
                         (if (= t h)
                           [t m]
                           (recur (nth nxt t) (nth nxt h) (inc m))))
            lam (loop [h (nth nxt entry) l 1]
                  (if (= h entry)
                    l
                    (recur (nth nxt h) (inc l))))]
        (str "CYCLE " mu " " lam " " entry)))))

(defn -main
  "Entry point for Floyd's cycle detection."
  [input]
  (let [toks (tokenize input)]
    (if (< (count toks) 2)
      (fail "invalid input: expected N and S")
      (let [n (parse-num (nth toks 0))
            s (parse-num (nth toks 1))]
        (if (or (nil? n) (nil? s) (<= n 0) (< s 0) (>= s n))
          (fail "invalid input: require 0 <= S < N")
          (let [nxt (parse-next toks n)]
            (if (nil? nxt)
              (fail "invalid input: expected N valid next pointers in [-1, N-1]")
              (println (floyd-detect nxt s)))))))))

(let [input (slurp *in*)]
  (when-not (clojure.string/blank? input)
    (-main input)))
