;; rod-cutting: extended bottom-up cut rod
;;
;; Bottom-up revenue DP: dp[i] is the best over first cuts j of
;; p[j] + dp[i-j]. Emits the optimal revenue.
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
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

(def min-long
  "Most negative long value: 1 shifted into the sign bit."
  (bit-shift-left 1 63))

(def max-long
  "Largest long value: every bit below the sign bit set."
  (bit-not min-long))

(defn parse-token
  "Parse tok as a decimal long integer with optional sign. Returns
   nil when tok is malformed or out of long range: the sign and ASCII
   digits are matched first, then parse-long reads the value."
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))

(defn read-longs
  "Read all of stdin as a vector of longs."
  []
  (mapv parse-token (tokenize (slurp *in*))))


(let [nums (read-longs)]
  (when (seq nums)
    (let [n (nth nums 0)
          p (mapv #(nth nums (inc %)) (range n))
          dp (loop [i 1 acc [0]]
               (if (> i n) acc
                 (recur (inc i) (conj acc (loop [j 1 best nil]
                                            (if (> j i) (or best 0)
                                              (recur (inc j) (if (and (<= j (count p))
                                                                       (or (nil? best) (> (+ (nth p (dec j)) (nth acc (- i j))) best)))
                                                                  (+ (nth p (dec j)) (nth acc (- i j))) best))))))))]
      (println (nth dp n)))))
