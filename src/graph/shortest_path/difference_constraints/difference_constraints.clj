;; difference-constraints: Bellman-Ford feasibility solver
;;
;; Super-source Bellman-Ford over x_j - x_i <= b edges: n rounds
;; of input-order relaxation, then a change test. Emits a solution
;; vector or INFEASIBLE.
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
          cs (mapv (fn [k] [(nth nums (+ 2 (* k 3))) (nth nums (+ 3 (* k 3))) (nth nums (+ 4 (* k 3)))]) (range m))
          d (atom (vec (repeat n 0)))]
      (doseq [_ (range n) [i j b] cs]
        (when (< (+ (nth @d i) b) (nth @d j))
          (swap! d assoc j (+ (nth @d i) b))))
      (if (some (fn [[i j b]] (< (+ (nth @d i) b) (nth @d j))) cs)
        (println "INFEASIBLE")
        (println (clojure.string/join " " @d))))))
