;; catalan: Catalan numbers and polygon triangulations
;;
;; Quadratic Catalan recurrence; triangulations of an m-gon equal
;; Catalan(m-2). Emits Catalan(n) then the m-gon count.
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
    (let [n (nth nums 0) m (nth nums 1)
          cat (fn [k]
                (loop [i 1 acc [1]]
                  (if (> i k) (nth acc k)
                    (recur (inc i) (conj acc (reduce + 0 (map * (subvec acc 0 i) (reverse (subvec acc 0 i)))))))))]
      (println (cat n))
      (println (if (>= m 2) (cat (- m 2)) 0)))))
