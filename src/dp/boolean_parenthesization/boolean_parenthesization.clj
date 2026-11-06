;; boolean-parenthesization: count true parenthesizations
;;
;; Interval DP: T[i][j]/F[i][j] count ways for the slice to be true
;; or false, split over every operator with &- |- ^ truth tables.
;; Emits the true count for the whole expression.
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


(let [toks (tokenize (slurp *in*))]
  (when (seq toks)
    (let [syms (nth toks 0)
          ops (if (> (count toks) 1) (nth toks 1) "")
          n (count syms)
          T (atom (mapv (fn [i] (mapv (fn [j] (if (and (= i j) (= (nth syms i) \T)) 1 0)) (range n))) (range n)))
          F (atom (mapv (fn [i] (mapv (fn [j] (if (and (= i j) (= (nth syms i) \F)) 1 0)) (range n))) (range n)))]
      (doseq [L (range 2 (inc n)) i (range (inc (- n L)))]
        (let [j (+ i L -1)
              [t f] (loop [k i t 0 ff 0]
                      (if (>= k j) [t ff]
                        (let [op (nth ops k)
                              lt (nth (nth @T i) k) lf (nth (nth @F i) k)
                              rt (nth (nth @T (inc k)) j) rf (nth (nth @F (inc k)) j)
                              tot (* (+ lt lf) (+ rt rf))
                              tt (cond (= op \&) (* lt rt)
                                       (= op \|) (- tot (* lf rf))
                                       :else (+ (* lt rf) (* lf rt)))]
                          (recur (inc k) (+ t tt) (+ ff (- tot tt))))))]
          (swap! T assoc-in [i j] t)
          (swap! F assoc-in [i j] f)))
      (println (nth (nth @T 0) (dec n))))))
