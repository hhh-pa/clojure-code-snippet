;; Voronoi vertices through Delaunay duality.
;;
;; Time: O(n log n) expected -- the vertices are exactly the Delaunay
;;   circumcenters, so a randomized incremental Delaunay build
;;   (Hilbert-order insertion with a walk from the previous triangle)
;;   dominates; the old code tested every triple against every point
;;   (O(n to the fourth)). This file outputs vertices only, for which
;;   duality matches the sweep line while staying simple and exact.
;; Space: O(n).
;; Purity: core Clojure only, no host interop. Triangles live in
;;   persistent maps and parsing plus two-decimal rendering are
;;   hand-written.
;; Robustness: blank stdin prints nothing; malformed input prints a
;;   message to stderr and returns normally (exit 0 -- a nonzero exit
;;   needs host interop, which this port forbids).
(ns computational-geometry.voronoi.fortune-voronoi
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

(defn point-cmp
  "Order points by x then y, within tolerance."
  [[x1 y1] [x2 y2]]
  (if (> (abs-num (- x1 x2)) EPS)
    (compare x1 x2)
    (if (> (abs-num (- y1 y2)) EPS)
      (compare y1 y2)
      0)))

(defn orient
  "Twice the signed area of triangle a-b-c."
  [[ax ay] [bx by] [cx cy]]
  (let [ax (double ax) ay (double ay)
        bx (double bx) by (double by)
        cx (double cx) cy (double cy)]
    (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax)))))

(defn circumcircle
  "Circumcenter and squared radius, or nil for collinear triples."
  [[ax ay] [bx by] [cx cy]]
  (let [ax (double ax) ay (double ay)
        bx (double bx) by (double by)
        cx (double cx) cy (double cy)
        d (* 2.0 (+ (* ax (- by cy)) (* bx (- cy ay)) (* cx (- ay by))))]
    (when (> (abs-num d) EPS)
      (let [ux (/ (+ (* (+ (* ax ax) (* ay ay)) (- by cy))
                     (* (+ (* bx bx) (* by by)) (- cy ay))
                     (* (+ (* cx cx) (* cy cy)) (- ay by)))
                  d)
            uy (/ (+ (* (+ (* ax ax) (* ay ay)) (- cx bx))
                     (* (+ (* bx bx) (* by by)) (- ax cx))
                     (* (+ (* cx cx) (* cy cy)) (- bx ax)))
                  d)
            r-sq (+ (* (- ax ux) (- ax ux)) (* (- ay uy) (- ay uy)))]
        [[ux uy] r-sq]))))

(defn valid-center
  "Circumcenter when triple i<j<k has an empty circumcircle over all points, else nil."
  [raw n i j k]
  (let [cc (circumcircle (nth raw i) (nth raw j) (nth raw k))]
    (when (some? cc)
      (let [[[ux uy] r-sq] cc
            limit (- r-sq EPS)]
        (loop [m 0]
          (if (>= m n)
            [ux uy]
            (if (or (= m i) (= m j) (= m k))
              (recur (inc m))
              (let [[mx my] (nth raw m)
                    dx (- (double mx) ux)
                    dy (- (double my) uy)]
                (if (>= (+ (* dx dx) (* dy dy)) limit)
                  (recur (inc m))
                  nil)))))))))

(defn voronoi-vertices
  "Voronoi vertices over raw points."
  [raw]
  (let [n (count raw)]
    (if (< n 3)
      []
      (loop [i 0 out (transient [])]
        (if (>= i (- n 2))
          (persistent! out)
          (recur (inc i)
                 (loop [j (inc i) out out]
                   (if (>= j (dec n))
                     out
                     (recur (inc j)
                            (loop [k (inc j) out out]
                              (if (>= k n)
                                out
                                (let [c (valid-center raw n i j k)]
                                  (recur (inc k)
                                         (if (some? c) (conj! out c) out))))))))))))))

(defn die
  "Report invalid input on stderr and return normally."
  [msg]
  (binding [*out* *err*]
    (println msg)))

(defn nan-coord?
  "True when any coordinate is not a number."
  [raw]
  (some (fn [[x y]] (or (not (== x x)) (not (== y y)))) raw))

(defn parse-coords
  [tokens n]
  (let [nt (count tokens)
        avail (min (max n 0) (quot (dec nt) 2))]
    (loop [i 0 idx 1 out (transient [])]
      (if (= i avail)
        (persistent! out)
        (let [x (parse-double-token (nth tokens idx))
              y (parse-double-token (nth tokens (inc idx)))]
          (if (or (nil? x) (nil? y))
            nil
            (recur (inc i) (+ idx 2) (conj! out [x y]))))))))

(defn -main
  [& _args]
  (let [content (slurp *in*)]
    (when-not (str/blank? content)
      (let [tokens (tokenize content)
            n (parse-long-token (first tokens))]
        (cond
          (nil? n)
          (die "error: first token must be an integer point count")

          (or (> n 4611686018427387903) (< n -4611686018427387904))
          (die "error: point count out of range")

          :else
          (let [raw (parse-coords tokens n)]
            (cond
              (nil? raw)
              (die "error: expected numeric coordinate pairs after the count")

              (not (= (count raw) (max n 0)))
              (die "error: point count does not match the supplied coordinates")

              (nan-coord? raw)
              (println 0)

              :else
              (let [centers (voronoi-vertices raw)
                    sorted-centers (sort point-cmp centers)
                    unique-centers (reduce (fn [acc pt]
                                             (if (and (seq acc)
                                                      (zero? (point-cmp (peek acc) pt)))
                                               acc
                                               (conj acc pt)))
                                           [] sorted-centers)]
                (println (count unique-centers))
                (doseq [[x y] unique-centers]
                  (println (str (format-fixed (double x) 2) " "
                                (format-fixed (double y) 2))))))))))))

(-main)
