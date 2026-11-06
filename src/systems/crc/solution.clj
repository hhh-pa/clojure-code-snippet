;; crc: polynomial modulo-2 division remainder (Tanenbaum networks).
;; Input: generator binary string; message binary string.
;; Output: remainder of length len(G)-1. Pure Clojure.

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

(defn solve [toks]
  (let [G (nth toks 0) M (nth toks 1)
        r (- (count G) 1)
        d0 (vec (concat (mapv (fn [c] (if (= c \1) 1 0)) M) (repeat r 0)))
        d (loop [i 0 dd d0]
            (if (>= i (count M)) dd
              (if (= (nth dd i) 1)
                (recur (+ i 1)
                       (loop [j 0 e dd]
                         (if (>= j (count G)) e
                           (recur (+ j 1) (if (= (nth G j) \1)
                                            (assoc e (+ i j) (bit-xor (nth e (+ i j)) 1)) e)))))
                (recur (+ i 1) dd))))]
    (apply str (map str (subvec d (count M))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks) (println (solve toks))))
