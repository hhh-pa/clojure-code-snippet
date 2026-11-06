;; viterbi: max log-probability path
;;
;; Viterbi over a topological pass: best[v] extends the best
;; predecessor by log edge probability. Emits the target score.
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
  (when (>= (count toks) 2)
    (let [n (parse-token (nth toks 0)) m (parse-token (nth toks 1))
          adj (atom (mapv (fn [_] []) (range n)))]
      (doseq [i (range m)]
        (let [u (parse-token (nth toks (+ 2 (* i 3))))
              v (parse-token (nth toks (+ 3 (* i 3))))
              p (Double/parseDouble (nth toks (+ 4 (* i 3))))]
          (swap! adj update u conj [v p])))
      (let [s (parse-token (nth toks (+ 2 (* m 3))))
            t (parse-token (nth toks (+ 3 (* m 3))))
            neg -1e100
            best (atom (assoc (vec (repeat n neg)) s 0.0))]
        (doseq [u (range n)]
          (when (not= (nth @best u) neg)
            (doseq [[v p] (nth @adj u)]
              (when (pos? p)
                (let [c (+ (nth @best u) (Math/log p))]
                  (when (> c (nth @best v))
                    (swap! best assoc v c)))))))
        (let [ans (nth @best t)]
          (println (String/format java.util.Locale/US "%.6f"
                       (to-array [(double (if (= ans neg) -1.0 ans))]))))))))
