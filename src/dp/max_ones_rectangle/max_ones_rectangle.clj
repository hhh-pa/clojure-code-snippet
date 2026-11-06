;; max-ones-square-rectangle: largest all-1 square and rectangle
;;
;; Square side by 3-neighbour DP; rectangle area by histogram
;; stacks per row. Emits the max square area then max rectangle area.
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
    (let [h (nth nums 0) w (nth nums 1)
          g (mapv (fn [r] (mapv (fn [c] (nth nums (+ 2 (* r w) c))) (range w))) (range h))
          dp (atom (mapv (fn [_] (mapv (fn [_] 0) (range w))) (range h)))
          ms (atom 0)]
      (doseq [r (range h) c (range w)]
        (when (pos? (nth (nth g r) c))
          (let [v (if (or (zero? r) (zero? c)) 1
                    (inc (min (nth (nth @dp (dec r)) c) (nth (nth @dp r) (dec c)) (nth (nth @dp (dec r)) (dec c)))))]
            (swap! dp assoc-in [r c] v)
            (swap! ms max v))))
      (let [hist (fn [hh]
                   (let [v (conj (vec hh) 0)]
                     (loop [i 0 st [] best 0]
                       (if (>= i (count v)) best
                         (let [[st2 best2] (loop [st st best best]
                                                     (if (and (seq st) (> (nth v (peek st)) (nth v i)))
                                                       (let [H (nth v (peek st)) stp (pop st)
                                                             Wd (if (empty? stp) i (- i (peek stp) 1))]
                                                         (recur stp (max best (* H Wd))))
                                                       [st best]))]
                           (recur (inc i) (conj st2 i) best2))))))
            he (atom (vec (repeat w 0)))
            mr (atom 0)]
        (doseq [r (range h)]
          (doseq [c (range w)]
            (swap! he assoc c (if (pos? (nth (nth g r) c)) (inc (nth @he c)) 0)))
          (swap! mr max (hist @he)))
        (println (* @ms @ms))
        (println @mr)))))
