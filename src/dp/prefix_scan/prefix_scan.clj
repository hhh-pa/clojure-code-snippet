;; prefix-scan: Blelloch up/down-sweep scan
;;
;; Padded Blelloch scan: up-sweep builds partial sums, down-sweep
;; distributes them into the exclusive scan. Emits exclusive then
;; inclusive scans.
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
  (when (>= (count nums) 1)
    (let [n (nth nums 0)
          a (mapv #(nth nums (inc %)) (range n))
          N (loop [x 1] (if (>= x (max n 1)) x (recur (* x 2))))
          t (atom (mapv #(if (< % n) (nth a %) 0) (range N)))]
      (loop [d 1]
        (when (< d N)
          (doseq [k (range 0 N (* 2 d))]
            (swap! t assoc (+ k (* 2 d) -1) (+ (nth @t (+ k (* 2 d) -1)) (nth @t (+ k d -1)))))
          (recur (* d 2))))
      (swap! t assoc (dec N) 0)
      (loop [d (quot N 2)]
        (when (>= d 1)
          (doseq [k (range 0 N (* 2 d))]
            (let [tmp (nth @t (+ k d -1))]
              (swap! t assoc (+ k d -1) (nth @t (+ k (* 2 d) -1)))
              (swap! t assoc (+ k (* 2 d) -1) (+ (nth @t (+ k (* 2 d) -1)) tmp))))
          (recur (quot d 2))))
      (println (clojure.string/join " " (subvec @t 0 n)))
      (println (clojure.string/join " " (map + (subvec @t 0 n) a))))))
