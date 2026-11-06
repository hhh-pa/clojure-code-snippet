;; egg-dropping: min worst-case drops for k eggs, n floors
;;
;; Moves-based DP: dp[e] tracks floors solvable with e eggs in m
;; moves via dp[e] += dp[e-1] + 1. Emits the smallest m reaching n.
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
  (when (>= (count nums) 2)
    (let [k (nth nums 0) n (nth nums 1)]
      (cond (<= n 0) (println 0)
            (<= k 0) (println -1)
            (= k 1) (println n)
            :else (let [dp (atom (vec (repeat (inc k) 0)))]
                    (loop [m 0]
                      (if (>= (nth @dp k) n)
                        (println m)
                        (do
                          (doseq [e (range k 0 -1)]
                            (let [v (+ (nth @dp e) (nth @dp (dec e)) 1)]
                              (swap! dp assoc e (if (> v (inc n)) (inc n) v))))
                          (recur (inc m))))))))))
