;; rt-scheduling: RMS Liu-Layland bound vs EDF utilization test (MOS 2.4.4).
;; Input: n; n lines C T. Output RMS YES/NO, EDF YES/NO.
;; 2^(1/n) via Newton iteration (uniform across languages). Pure Clojure.

(def digit-values
  (zipmap "0123456789" (range)))

(def separators
  #{\space \tab \newline \return \formfeed \u000B})

(defn tokenize
  [s]
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

(def min-long (bit-shift-left 1 63))
(def max-long (bit-not min-long))

(defn parse-token
  [tok]
  (when (and (string? tok) (re-matches #"[+-]?[0-9]+" tok))
    (parse-long tok)))

(defn read-int [tok] (parse-token tok))

(defn npow2inv [n]
  (loop [it 0 x 1.0]
    (if (>= it 200) x
      (let [p (loop [i 0 p 1.0] (if (>= i (- n 1)) p (recur (+ i 1) (* p x))))
            f (- (* p x) 2.0)
            d (* (double n) p)]
        (recur (+ it 1) (- x (/ f d)))))))

(defn solve [toks]
  (let [n (read-int (nth toks 0))
        u (loop [i 0 acc 0.0]
            (if (>= i n) acc
              (recur (+ i 1) (+ acc (/ (double (read-int (nth toks (+ 1 (* i 2)))))
                                       (double (read-int (nth toks (+ 2 (* i 2))))))))))
        bound (* (double n) (- (npow2inv n) 1.0))]
    [(if (<= u bound) "RMS YES" "RMS NO")
     (if (<= u 1.0) "EDF YES" "EDF NO")]))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (doseq [line (solve toks)] (println line))))
