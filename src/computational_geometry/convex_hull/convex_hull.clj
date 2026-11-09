;; Hull perimeter by monotone chain.
;;
;; Time: O(n log n) -- sorting dominates; worst-case optimal because any
;;   hull algorithm sorts in the worst case.
;; Space: O(n) for the sorted distinct points and the hull.
;; Purity: core Clojure only, no host interop.
(ns computational-geometry.convex-hull.convex-hull
  (:require [clojure.string :as str]))

(defn tokenize
  "Split s on ASCII whitespace."
  [s]
  (if (nil? s)
    []
    (let [v (str/split s #"[ \t\n\r\f\x0B]+")
          n (count v)
          start (if (and (pos? n) (str/blank? (nth v 0))) 1 0)
          end (if (and (> n start) (str/blank? (nth v (dec n)))) (dec n) n)]
      (subvec v start end))))

(defn parse-long-token
  "Parse an integer token; nil when malformed or outside int64 range."
  [tok]
  (when (string? tok)
    (parse-long tok)))

(defn parse-int32-token
  "Parse an integer token limited to int32 range; nil otherwise."
  [tok]
  (let [v (parse-long-token tok)]
    (when (and (some? v) (<= -2147483648 v 2147483647))
      v)))

(defn sqrt
  "Newton square root for a nonnegative finite double."
  [x]
  (cond
    (zero? x) x
    (not (== x x)) x
    (== x (* x 2.0)) x
    (neg? x) (/ 0.0 0.0)
    :else
    (let [[m adj] (loop [m x adj 1.0]
                    (cond (> m 4.0) (recur (/ m 4.0) (* adj 2.0))
                          (< m 0.25) (recur (* m 4.0) (/ adj 2.0))
                          :else [m adj]))
          g (loop [i 0 g (if (> m 1.0) m 1.0)]
              (let [g2 (/ (+ g (/ m g)) 2.0)]
                (if (or (== g g2) (= i 10)) g2 (recur (inc i) g2))))]
      (* g adj))))

(defn double-to-bigint
  "Exact integer value of an integral double beyond long range."
  [v]
  (loop [q v k 0]
    (if (< q 9007199254740992.0)
      (loop [i 0 acc (bigint (long q))]
        (if (= i k) acc (recur (inc i) (*' acc 2))))
      (recur (/ q 2.0) (inc k)))))

(defn format-fixed-even
  "Render x with exactly one decimal, ties to even."
  [x]
  (cond
    (not (== x x)) "NaN"
    (and (not (zero? x)) (== x (* x 2.0)))
    (if (neg? x) "-Infinity" "Infinity")
    :else
    (let [neg (neg? (/ 1.0 x))
          a (if neg (- x) x)]
      (if (>= a 9.0e18)
        (str (when neg "-") (str (double-to-bigint a)) ".0")
        (let [ip (long a)
              frac (- a (double ip))
              scaled (* frac 10.0)
              fl (long scaled)
              f (- scaled (double fl))
              r (cond (> f 0.5) (inc fl)
                      (< f 0.5) fl
                      (even? fl) fl
                      :else (inc fl))
              [ip r] (if (= r 10) [(inc ip) 0] [ip r])]
          (str (when neg "-") (str ip) "." (str r)))))))

(defn pack-point
  "Pack int32 point (x y) into one long whose signed order matches
   lexicographic point order (bias to unsigned, then flip the sign
   bit), so points sort as plain longs."
  [x y]
  (bit-xor (bit-or (bit-shift-left (+ x 2147483648) 32)
                   (bit-and (+ y 2147483648) 4294967295))
           -9223372036854775808))

(defn chain
  "One monotone chain over sorted distinct packed points, minus its last point."
  [pts]
  (let [m (count pts)
        h (transient [])]
    (loop [i 0]
      (if (= i m)
        (do
          (when (pos? (count h))
            (pop! h))
          (persistent! h))
        (let [p (nth pts i)
              px (- (unsigned-bit-shift-right (bit-xor p -9223372036854775808) 32) 2147483648)
              py (- (bit-and (bit-xor p -9223372036854775808) 4294967295) 2147483648)]
          (loop []
            (when (>= (count h) 2)
              (let [hn (count h)
                    b (nth h (dec hn))
                    a (nth h (- hn 2))
                    ax (- (unsigned-bit-shift-right (bit-xor a -9223372036854775808) 32) 2147483648)
                    ay (- (bit-and (bit-xor a -9223372036854775808) 4294967295) 2147483648)
                    bx (- (unsigned-bit-shift-right (bit-xor b -9223372036854775808) 32) 2147483648)
                    by (- (bit-and (bit-xor b -9223372036854775808) 4294967295) 2147483648)
                    cr (-' (*' (-' bx ax) (-' py ay)) (*' (-' by ay) (-' px ax)))]
                (when (<= cr 0)
                  (pop! h)
                  (recur)))))
          (conj! h p)
          (recur (inc i)))))))

(defn dist
  "Euclidean distance between two packed integer points."
  [p1 p2]
  (let [dx (double (-' (- (unsigned-bit-shift-right (bit-xor p1 -9223372036854775808) 32) 2147483648) (- (unsigned-bit-shift-right (bit-xor p2 -9223372036854775808) 32) 2147483648)))
        dy (double (-' (- (bit-and (bit-xor p1 -9223372036854775808) 4294967295) 2147483648) (- (bit-and (bit-xor p2 -9223372036854775808) 4294967295) 2147483648)))]
    (sqrt (+ (* dx dx) (* dy dy)))))

(defn perimeter
  "Cyclic perimeter of the hull polygon."
  [hull]
  (let [m (count hull)]
    (loop [i 0 acc 0.0]
      (if (>= i m)
        acc
        (recur (inc i) (+ acc (dist (nth hull i)
                                    (nth hull (rem (inc i) m)))))))))

(defn die
  "Report invalid input on stderr and return normally."
  [msg]
  (binding [*out* *err*]
    (println msg)))

(defn dedup-sorted
  "Deduplicate a sorted vector of points in O(n) without hashing."
  [sv]
  (let [m (count sv)]
    (if (<= m 1)
      sv
      (loop [i 1 prev (nth sv 0) out (conj! (transient []) (nth sv 0))]
        (if (= i m)
          (persistent! out)
          (let [cur (nth sv i)]
            (if (= cur prev)
              (recur (inc i) prev out)
              (recur (inc i) cur (conj! out cur)))))))))

(defn -main
  [& _args]
  (let [content (slurp *in*)]
    (when-not (str/blank? content)
      (let [tokens (tokenize content)
            nt (count tokens)
            n (parse-long-token (first tokens))]
        (cond
          (nil? n)
          (die "error: first token must be an integer point count")

          (or (neg? n) (> n 2147483647))
          (die "error: point count out of range")

          (< (dec nt) (*' 2 n))
          (die "error: expected integer coordinate pairs after the count")

          :else
          (let [raw (loop [i 0 acc (transient [])]
                      (if (= i n)
                        (persistent! acc)
                        (let [idx (inc (* 2 i))
                              a (parse-int32-token (nth tokens idx))
                              b (parse-int32-token (nth tokens (inc idx)))]
                          (if (or (nil? a) (nil? b))
                            nil
                            (recur (inc i) (conj! acc (pack-point a b)))))))]
            (if (nil? raw)
              (die "error: expected integer coordinate pairs after the count")
              (let [pts (dedup-sorted (vec (sort raw)))
                    u (count pts)]
                (if (< u 2)
                  (println "0.0")
                  (let [hull (into (chain pts) (chain (vec (rseq pts))))]
                    (println (format-fixed-even (perimeter hull)))))))))))))

(-main)
