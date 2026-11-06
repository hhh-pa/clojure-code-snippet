;; box-stacking: max stack height over 3D rotations
;;
;; Expands every box into three rotations, sorts by base area, then
;; LIS-style DP with strict base dominance. Emits the max height.
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
  (if (empty? nums)
    (println 0)
    (let [n (nth nums 0)
          boxes (mapv (fn [i] [(nth nums (+ 1 (* i 3))) (nth nums (+ 2 (* i 3))) (nth nums (+ 3 (* i 3)))]) (range n))
          rots (vec (mapcat (fn [[x y z]] (map (fn [[a b h]] (if (>= a b) [a b h] [b a h]))
                                                      [[x y z] [x z y] [y z x]])) boxes))
          srt (vec (sort-by (fn [[w d _]] [(- (* w d)) (- w) (- d)]) rots))
          m (count srt)
          dp (atom (mapv (fn [i] (nth (nth srt i) 2)) (range m)))]
      (doseq [i (range m) j (range i)]
        (when (and (> (nth (nth srt j) 0) (nth (nth srt i) 0))
                    (> (nth (nth srt j) 1) (nth (nth srt i) 1)))
          (swap! dp assoc i (max (nth @dp i) (+ (nth @dp j) (nth (nth srt i) 2))))))
      (println (reduce max 0 @dp)))))
