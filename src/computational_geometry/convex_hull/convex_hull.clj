;; Hull perimeter by monotone chain.
;;
;; Time: O(n log n) -- sorting dominates; worst-case optimal because any
;;   hull algorithm sorts in the worst case.
;; Space: O(n) for the sorted distinct points and the hull.
;; Purity: core Clojure only, no host interop. The old packed integer tables
;;   and BigDecimal rendering are replaced by persistent vectors, exact
;;   integer arithmetic and a hand-written one-decimal renderer (ties to
;;   even, matching the old output).
;; Robustness: blank stdin prints nothing; malformed input prints a
;;   message to stderr and returns normally (exit 0 -- a nonzero exit
;;   needs host interop, which this port forbids).
(ns computational-geometry.convex-hull.convex-hull
  (:require [clojure.string :as str]))

(def digit-values
  "Digit character to numeric value."
  (zipmap "0123456789" (range)))

(defn whitespace?
  "True for the ASCII whitespace that separates input tokens."
  [ch]
  (or (= ch \space) (= ch \tab) (= ch \newline) (= ch \return)
      (= ch \formfeed) (= ch (char 11))))

(defn tokenize
  "Split s on ASCII whitespace."
  [s]
  (let [[toks cur] (reduce (fn [[toks cur] ch]
                             (if (whitespace? ch)
                               [(if (seq cur) (conj toks (apply str cur)) toks) []]
                               [toks (conj cur ch)]))
                           [[] []]
                           (seq s))]
    (if (seq cur) (conj toks (apply str cur)) toks)))

(defn parse-long-token
  "Parse an integer token; nil when malformed or outside int64 range."
  [tok]
  (let [cs (vec tok)
        n (count cs)]
    (when (pos? n)
      (let [c0 (nth cs 0)
            neg (or (= c0 \-) (= c0 \+))
            want-neg (= c0 \-)
            digits (subvec cs (if neg 1 0))]
        (when (and (seq digits) (every? digit-values digits))
          (let [stripped (vec (drop-while #(= % \0) digits))
                len (count stripped)
                lim (if want-neg "9223372036854775808" "9223372036854775807")]
            (when (or (< len 19)
                      (and (= len 19) (<= (compare (apply str stripped) lim) 0)))
              (let [v (reduce (fn [acc ch] (- (* acc 10) (digit-values ch)))
                              0 stripped)]
                (if want-neg v (- v))))))))))

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
              (if (= i 30) g (recur (inc i) (/ (+ g (/ m g)) 2.0))))]
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

(defn cross
  "Twice the signed area of triangle o-a-b; exact integer math."
  [[ox oy] [ax ay] [bx by]]
  (-' (*' (-' ax ox) (-' by oy)) (*' (-' ay oy) (-' bx ox))))

(defn chain
  "One monotone chain over sorted distinct points, minus its last point."
  [pts]
  (let [h (reduce (fn [h p]
                    (loop [h h]
                      (if (and (>= (count h) 2)
                               (<= (cross (nth h (- (count h) 2)) (peek h) p) 0))
                        (recur (pop h))
                        (conj h p))))
                  [] pts)]
    (if (seq h) (pop h) h)))

(defn dist
  "Euclidean distance between two integer points."
  [[x1 y1] [x2 y2]]
  (let [dx (double (- x1 x2))
        dy (double (- y1 y2))]
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

(defn -main
  [& _args]
  (let [content (slurp *in*)]
    (when-not (str/blank? content)
      (let [tokens (tokenize content)
            n (parse-long-token (first tokens))]
        (cond
          (nil? n)
          (die "error: first token must be an integer point count")

          (or (neg? n) (> n 2147483647))
          (die "error: point count out of range")

          :else
          (let [coords (take (*' 2 n) (rest tokens))
                raw (vec (for [[xs ys] (partition 2 coords)]
                           [(parse-int32-token xs) (parse-int32-token ys)]))]
            (if (some (fn [[a b]] (or (nil? a) (nil? b))) raw)
              (die "error: expected integer coordinate pairs after the count")
              (let [pts (vec (distinct (sort raw)))
                    u (count pts)]
                (if (< u 2)
                  (println "0.0")
                  (let [hull (into (chain pts) (chain (rseq pts)))]
                    (println (format-fixed-even (perimeter hull)))))))))))))

(-main)
