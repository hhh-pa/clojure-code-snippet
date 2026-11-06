;; transitive-closure: Warshall reachability closure
;;
;; Warshall triple loop with k outermost over a reflexive seed
;; matrix. Emits the 0/1 reachability rows.
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
          t (atom (mapv (fn [i] (mapv (fn [j] (if (= i j) 1 0)) (range n))) (range n)))]
      (doseq [i (range m)]
        (swap! t assoc-in [(nth nums (+ 2 (* i 2))) (nth nums (+ 3 (* i 2)))] 1))
      (doseq [k (range n) i (range n)]
        (when (= (nth (nth @t i) k) 1)
          (doseq [j (range n)]
            (when (= (nth (nth @t k) j) 1)
              (swap! t assoc-in [i j] 1)))))
      (doseq [i (range n)]
        (println (clojure.string/join " " (nth @t i)))))))
