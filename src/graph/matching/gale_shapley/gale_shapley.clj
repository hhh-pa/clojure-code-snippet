;; gale-shapley: stable marriage by men-proposing
;;
;; Men propose down their lists; women keep the best suitor seen.
;; Emits the man-optimal stable pairs as m w lines.
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
          mp (mapv (fn [i] (mapv (fn [j] (nth nums (+ 1 (* i n) j))) (range n))) (range n))
          wp (mapv (fn [i] (mapv (fn [j] (nth nums (+ 1 (* n n) (* i n) j))) (range n))) (range n))
          rank (mapv (fn [w] (reduce (fn [acc r] (assoc acc (nth (nth wp w) r) r)) (vec (repeat n 0)) (range n))) (range n))
          wife (atom (vec (repeat n -1)))
          husb (atom (vec (repeat n -1)))
          nxt (atom (vec (repeat n 0)))]
      (loop [free (into clojure.lang.PersistentQueue/EMPTY (range n))]
        (when (seq free)
          (let [m (peek free)
                w (nth (nth mp m) (nth @nxt m))]
            (swap! nxt assoc m (inc (nth @nxt m)))
            (cond (= (nth @husb w) -1)
                  (do (swap! wife assoc m w) (swap! husb assoc w m)
                      (recur (pop free)))
                  (< (nth (nth rank w) m) (nth (nth rank w) (nth @husb w)))
                  (let [ex (nth @husb w)]
                    (swap! wife assoc ex -1)
                    (swap! wife assoc m w)
                    (swap! husb assoc w m)
                    (recur (conj (pop free) ex)))
                  :else (recur (conj (pop free) m))))))
      (doseq [m (range n)]
        (println (str m " " (nth @wife m)))))))
