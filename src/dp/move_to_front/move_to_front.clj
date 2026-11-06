;; move-to-front: self-organizing list heuristic
;;
;; List starts sorted; each access pays its 1-based position and
;; moves the item to the front. Emits the total access cost.
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
          req (mapv #(nth nums (+ 2 %)) (range m))]
      (loop [rs req lst (vec (range n)) tot 0]
        (if (empty? rs)
          (println tot)
          (let [x (first rs)
                i (loop [j 0] (if (= (nth lst j) x) j (recur (inc j))))]
            (recur (rest rs) (vec (cons x (concat (subvec lst 0 i) (subvec lst (inc i))))) (+ tot (inc i)))))))))
