;; josephus: (n,m)-elimination via order-statistic Fenwick tree (CLRS 17).
;; O(n log n). Input: n m. Output: elimination order, 1-based. Pure Clojure.

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

(defn bit-add [bit n i d]
  (loop [bit bit i i]
    (if (> i n) bit
      (recur (assoc bit i (+ (nth bit i) d)) (+ i (bit-and i (- i)))))))

(defn bit-kth [bit n k]
  (let [step (loop [s 1] (if (<= (* s 2) n) (recur (* s 2)) s))]
    (loop [i 0 st step k k]
      (if (zero? st) (+ i 1)
        (let [j (+ i st)]
          (if (and (<= j n) (< (nth bit j) k))
            (recur j (quot st 2) (- k (nth bit j)))
            (recur i (quot st 2) k)))))))

(defn solve [toks]
  (let [n (read-int (nth toks 0)) m (read-int (nth toks 1))
        bit0 (loop [i 1 b (vec (repeat (+ n 1) 0))]
               (if (> i n) b (recur (+ i 1) (bit-add b n i 1))))]
    (loop [pos 0 rem n bit bit0 out []]
      (if (zero? rem) out
        (let [p (mod (+ pos m -1) rem)
              idx (bit-kth bit n (+ p 1))]
          (recur p (- rem 1) (bit-add bit n idx -1) (conj out idx)))))))

(let [toks (tokenize (slurp *in*))]
  (when (seq toks)
    (println (apply str (interpose " " (map str (solve toks)))))))
