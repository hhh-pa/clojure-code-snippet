;; matrix-multiply: recursive 8-way parallel multiplication
;;
;; P-Matrix-Multiply-Recursive: each quadrant combines two half-size
;; products; the sequential base case multiplies directly. Emits the
;; product rows.
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
          N (loop [x 1] (if (>= x (max n 1)) x (recur (* x 2))))
          A (atom (mapv (fn [i] (mapv (fn [j] (if (and (< i n) (< j n)) (nth nums (+ 1 (* i n) j)) 0)) (range N))) (range N)))
          B (atom (mapv (fn [i] (mapv (fn [j] (if (and (< i n) (< j n)) (nth nums (+ 1 (* n n) (* i n) j)) 0)) (range N))) (range N)))
          C (atom (mapv (fn [_] (mapv (fn [_] 0) (range N))) (range N)))]
      (letfn [(rec [a0 a1 b0 b1 c0 c1 s]
                (if (<= s 2)
                  (doseq [i (range s) k (range s) j (range s)]
                    (swap! C assoc-in [(+ c0 i) (+ c1 j)]
                      (+ (nth (nth @C (+ c0 i)) (+ c1 j))
                         (* (nth (nth @A (+ a0 i)) (+ a1 k)) (nth (nth @B (+ b0 k)) (+ b1 j))))))
                  (let [h (quot s 2)]
                    (rec a0 a1 b0 b1 c0 c1 h) (rec a0 (+ a1 h) (+ b0 h) b1 c0 c1 h)
                    (rec a0 a1 b0 (+ b1 h) c0 (+ c1 h) h) (rec a0 (+ a1 h) (+ b0 h) (+ b1 h) c0 (+ c1 h) h)
                    (rec (+ a0 h) a1 b0 b1 (+ c0 h) c1 h) (rec (+ a0 h) (+ a1 h) (+ b0 h) b1 (+ c0 h) c1 h)
                    (rec (+ a0 h) a1 b0 (+ b1 h) (+ c0 h) (+ c1 h) h)
                    (rec (+ a0 h) (+ a1 h) (+ b0 h) (+ b1 h) (+ c0 h) (+ c1 h) h))))]
        (rec 0 0 0 0 0 0 N)
        (doseq [i (range n)]
          (println (clojure.string/join " " (subvec (nth @C i) 0 n))))))))
