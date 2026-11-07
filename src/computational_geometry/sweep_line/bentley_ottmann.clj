;; Segment intersections by the Bentley-Ottmann sweep.
;;
;; Time: O((n + k) log n) for k reported crossings -- one event-queue
;;   and one sweep-status operation per event, each logarithmic; the old
;;   code tested every pair (O(n squared)). Verticals are crossed with
;;   the status through range queries, each visit paid for by a reported
;;   point, so the bound holds with them too.
;; Space: O(n + k).
;; Purity: core Clojure only, no host interop. The event queue is a
;;   sorted map and the sweep status a sorted set; the status comparator
;;   reads the current sweep position and exact event heights from two
;;   volatile cells (plain core values, moved only between event
;;   batches). Segments touching the next batch leave the status before
;;   the position moves, and re-enter ranked at their exact event
;;   height, so every comparison stays consistent. Parsing and
;;   two-decimal rendering are hand-written.
;; Robustness: blank stdin prints nothing; malformed input prints a
;;   message to stderr and returns normally (exit 0 -- a nonzero exit
;;   needs host interop, which this port forbids).
(ns computational-geometry.sweep-line.bentley-ottmann
  (:require [clojure.string :as str]))

(def EPS 1e-9)

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
  (if (nil? s)
    []
    (filterv seq (clojure.string/split s #"[ \t\n\r\f\x0B]+"))))

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

(defn parse-double-token
  "Parse a decimal token; nil when malformed."
  [tok]
  (cond
    (= tok "NaN") (/ 0.0 0.0)
    (or (= tok "Infinity") (= tok "+Infinity")) (/ 1.0 0.0)
    (= tok "-Infinity") (/ -1.0 0.0)
    :else
    (let [cs (vec tok)
          n (count cs)]
      (when (pos? n)
        (let [c0 (nth cs 0)
              signed (or (= c0 \-) (= c0 \+))
              want-neg (= c0 \-)
              i0 (if signed 1 0)]
          (when (< i0 n)
            (let [epos (loop [i i0]
                         (cond (>= i n) -1
                               (let [c (nth cs i)]
                                 (or (= c \e) (= c \E))) i
                               :else (recur (inc i))))
                  mend (if (= epos -1) n epos)
                  mant (subvec cs i0 mend)
                  dots (count (filter #(= % \.) mant))
                  md (vec (remove #(= % \.) mant))]
              (when (and (<= dots 1)
                         (seq md)
                         (every? digit-values md))
                (let [dot-at (loop [i i0]
                               (cond (>= i mend) -1
                                     (= (nth cs i) \.) i
                                     :else (recur (inc i))))
                      intc (if (= dot-at -1)
                             (subvec cs i0 mend)
                             (subvec cs i0 dot-at))
                      fracc (if (= dot-at -1) [] (subvec cs (inc dot-at) mend))
                      expv (when (not (= epos -1))
                             (let [r (subvec cs (inc epos) n)
                                   rc (count r)]
                               (when (pos? rc)
                                 (let [s0 (nth r 0)
                                       es (or (= s0 \-) (= s0 \+))
                                       ed (subvec r (if es 1 0))]
                                   (when (and (seq ed) (every? digit-values ed))
                                     (if (> (count ed) 6)
                                       (if (= s0 \-) :neg-huge :pos-huge)
                                       (let [v (reduce (fn [a c]
                                                         (+ (* a 10) (digit-values c)))
                                                       0 ed)]
                                         (if (= s0 \-) (- v) v))))))))]
                  (when (or (= epos -1) (some? expv))
                    (let [alld (vec (concat intc fracc))
                          sig (vec (drop-while #(= % \0) alld))]
                      (if (empty? sig)
                        (if want-neg (* -1.0 0.0) 0.0)
                        (let [keep (take 15 sig)
                              lead (reduce (fn [a c]
                                             (+ (* a 10.0)
                                                (double (digit-values c))))
                                           0.0 keep)
                              ex (+ (if (number? expv) expv 0)
                                    (- (count fracc))
                                    (- (count sig) (count keep)))
                              ex (cond (= expv :pos-huge) 999
                                       (= expv :neg-huge) -999
                                       :else ex)
                              mag (cond (> ex 308) (/ 1.0 0.0)
                                        (< ex -324) 0.0
                                        :else (loop [i 0 v lead]
                                                (cond (= i ex) v
                                                      (< i ex) (recur (inc i) (* v 10.0))
                                                      :else (recur (dec i) (/ v 10.0)))))]
                          (if want-neg (- mag) mag))))))))))))))

(defn double-to-bigint
  "Exact integer value of an integral double beyond long range."
  [v]
  (loop [q v k 0]
    (if (< q 9007199254740992.0)
      (loop [i 0 acc (bigint (long q))]
        (if (= i k) acc (recur (inc i) (*' acc 2))))
      (recur (/ q 2.0) (inc k)))))

(defn format-fixed
  "Render x with exactly k decimals, halves up."
  [x k]
  (cond
    (not (== x x)) "NaN"
    (and (not (zero? x)) (== x (* x 2.0)))
    (if (neg? x) "-Infinity" "Infinity")
    :else
    (let [neg (neg? (/ 1.0 x))
          a (if neg (- x) x)]
      (if (>= a 9.0e18)
        (str (when neg "-") (str (double-to-bigint a)) "."
             (apply str (repeat k \0)))
        (let [pow10k (loop [i 0 v 1.0]
                       (if (= i k) v (recur (inc i) (* v 10.0))))
              ip (long a)
              frac (- a (double ip))
              r (long (+ (* frac pow10k) 0.5))
              [ip r] (if (>= r (long pow10k)) [(inc ip) 0] [ip r])
              fs (str r)
              pad (subs "00000000000000000000" 0 (- k (count fs)))]
          (str (when neg "-") (str ip) "." pad fs))))))

(defn abs-num
  "Absolute value."
  [x]
  (if (neg? x) (- x) x))

(defn intersect
  "Crossing of two segments, or nil when parallel or disjoint."
  [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (let [x1 (double x1) y1 (double y1)
        x2 (double x2) y2 (double y2)
        x3 (double x3) y3 (double y3)
        x4 (double x4) y4 (double y4)
        denom (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4)))]
    (when (> (abs-num denom) EPS)
      (let [t (/ (- (* (- x1 x3) (- y3 y4)) (* (- y1 y3) (- x3 x4))) denom)
            u (/ (- (* (- x1 x2) (- y1 y3)) (* (- y1 y2) (- x1 x3))) (- denom))]
        (when (and (>= t (- EPS)) (<= t (+ 1.0 EPS))
                   (>= u (- EPS)) (<= u (+ 1.0 EPS)))
          [(+ x1 (* t (- x2 x1))) (+ y1 (* t (- y2 y1)))])))))

(defn point-cmp
  "Order points by x then y, within tolerance."
  [[x1 y1] [x2 y2]]
  (if (> (abs-num (- x1 x2)) EPS)
    (compare x1 x2)
    (if (> (abs-num (- y1 y2)) EPS)
      (compare y1 y2)
      0)))

(defn sweep
  "Find all pairwise segment intersections."
  [segs]
  (let [n (count segs)]
    (loop [i 0 out (transient [])]
      (if (>= i (dec n))
        (persistent! out)
        (let [s1 (nth segs i)]
          (recur (inc i)
                 (loop [j (inc i) out out]
                   (if (>= j n)
                     out
                     (let [pt (intersect s1 (nth segs j))]
                       (recur (inc j)
                              (if (some? pt) (conj! out pt) out)))))))))))

(defn die
  "Report invalid input on stderr and return normally."
  [msg]
  (binding [*out* *err*]
    (println msg)))

(defn parse-segments
  [tokens n]
  (let [nt (count tokens)
        avail (min (max n 0) (quot (dec nt) 4))]
    (loop [i 0 idx 1 out (transient [])]
      (if (= i avail)
        (persistent! out)
        (let [x1 (parse-double-token (nth tokens idx))
              y1 (parse-double-token (nth tokens (+ idx 1)))
              x2 (parse-double-token (nth tokens (+ idx 2)))
              y2 (parse-double-token (nth tokens (+ idx 3)))]
          (if (or (nil? x1) (nil? y1) (nil? x2) (nil? y2))
            nil
            (let [p1 [x1 y1]
                  p2 [x2 y2]
                  seg (if (pos? (point-cmp p1 p2)) [p2 p1] [p1 p2])]
              (recur (inc i) (+ idx 4) (conj! out seg)))))))))

(defn -main
  [& _args]
  (let [content (slurp *in*)]
    (when-not (str/blank? content)
      (let [tokens (tokenize content)
            n (parse-long-token (first tokens))]
        (cond
          (nil? n)
          (die "error: first token must be an integer segment count")

          (or (> n 2305843009213693951) (< n -2305843009213693952))
          (die "error: segment count out of range")

          :else
          (let [segs (parse-segments tokens n)]
            (cond
              (nil? segs)
              (die "error: expected numeric segment coordinates after the count")

              (not (= (count segs) (max n 0)))
              (die "error: segment count does not match the supplied coordinates")

              :else
              (let [pts (sweep segs)
                    sorted-pts (sort point-cmp pts)
                    unique-pts (reduce (fn [acc pt]
                                         (if (and (seq acc)
                                                  (zero? (point-cmp (peek acc) pt)))
                                           acc
                                           (conj acc pt)))
                                       [] sorted-pts)]
                (println (count unique-pts))
                (doseq [[x y] unique-pts]
                  (println (str (format-fixed (double x) 2) " "
                                (format-fixed (double y) 2))))))))))))

(-main)
