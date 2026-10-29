;; Polygon triangulation by ear clipping.
;;
;; Time: O(n cubed) in the worst case; Space: O(n).
;; Justification for not reaching O(n log n): the hard output contract pins
;;   this file to byte-identical diagonals with the old ear-clipping
;;   output, and any asymptotically faster triangulation (for example
;;   monotone decomposition) emits different diagonals on the same
;;   polygon. Among implementations that reproduce this exact diagonal
;;   order the classic scan is kept; only host interop is removed.
;; Purity: core Clojure only, no host interop. Manual tokenizer and
;;   number parser; predicates and scan order unchanged.
;; Robustness: blank stdin prints nothing; malformed input prints a
;;   message to stderr and returns normally (exit 0 -- a nonzero exit
;;   needs host interop, which this port forbids).
(ns computational-geometry.triangulation.polygon-triangulation
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

(defn cross
  "Twice the signed area of triangle a-b-c."
  [a b c]
  (let [ax (double (:x a)) ay (double (:y a))
        bx (double (:x b)) by (double (:y b))
        cx (double (:x c)) cy (double (:y c))]
    (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax)))))

(defn point-in-triangle?
  "True when p lies inside triangle a-b-c or on its border."
  [p a b c]
  (let [cp1 (cross a b p)
        cp2 (cross b c p)
        cp3 (cross c a p)
        has-neg (or (< cp1 (- EPS)) (< cp2 (- EPS)) (< cp3 (- EPS)))
        has-pos (or (> cp1 EPS) (> cp2 EPS) (> cp3 EPS))]
    (not (and has-neg has-pos))))

(defn emit
  "Print triangles sorted by index triple."
  [triangles]
  (let [sorted-tris (sort-by (fn [[i j k]] [i j k]) triangles)]
    (println (count sorted-tris))
    (doseq [[i j k] sorted-tris]
      (println (str i " " j " " k)))))

(defn find-ear
  "Index and triangle of the first ear scanning from 0; nil when none."
  [cur-poly]
  (let [sz (count cur-poly)]
    (loop [i 0]
      (when (< i sz)
        (let [prev-i (rem (+ i sz -1) sz)
              next-i (rem (inc i) sz)
              a (nth cur-poly prev-i)
              b (nth cur-poly i)
              c (nth cur-poly next-i)]
          (if (<= (cross a b c) EPS)
            (recur (inc i))
            (let [clear? (every? (fn [j]
                                   (if (or (= j prev-i) (= j i) (= j next-i))
                                     true
                                     (not (point-in-triangle? (nth cur-poly j)
                                                              a b c))))
                                 (range sz))]
              (if clear?
                [i (vec (sort [(:id a) (:id b) (:id c)]))]
                (recur (inc i))))))))))

(defn triangulate
  "Ear-clipping loop; prints the finished (or partial) triangulation."
  [poly]
  (loop [cur-poly poly
         triangles []]
    (if (<= (count cur-poly) 3)
      (emit (if (= (count cur-poly) 3)
              (conj triangles (vec (sort (map :id cur-poly))))
              triangles))
      (let [ear (find-ear cur-poly)]
        (if-not ear
          (emit triangles)
          (let [[ear-idx tri] ear
                next-poly (vec (concat (subvec cur-poly 0 ear-idx)
                                       (subvec cur-poly (inc ear-idx))))]
            (recur next-poly (conj triangles tri))))))))

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

          (or (> n 4611686018427387903) (< n -4611686018427387904))
          (die "error: point count out of range")

          :else
          (let [coords (take (* 2 n) (rest tokens))
                raw (vec (for [[xs ys] (partition 2 coords)]
                           [(parse-double-token xs) (parse-double-token ys)]))]
            (cond
              (some (fn [[a b]] (or (nil? a) (nil? b))) raw)
              (die "error: expected numeric coordinate pairs after the count")

              (not (= (count raw) (max n 0)))
              (die "error: point count does not match the supplied coordinates")

              :else
              (let [poly (vec (map-indexed (fn [i [x y]] {:x x :y y :id i}) raw))
                    signed-area (loop [i 0 acc 0.0]
                                  (if (>= i n)
                                    acc
                                    (let [ni (rem (inc i) n)]
                                      (recur (inc i)
                                             (+ acc
                                                (- (* (:x (nth poly i))
                                                      (:y (nth poly ni)))
                                                   (* (:x (nth poly ni))
                                                      (:y (nth poly i)))))))))
                    poly (if (< signed-area 0.0) (vec (reverse poly)) poly)]
                (triangulate poly)))))))))

(-main)
