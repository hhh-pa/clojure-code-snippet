;; Convex hull by Chan's algorithm.
;;
;; Time: O(n log h) worst case for h hull vertices -- groups of size m
;;   are hulled in O(n log m), then gift-wrapping takes m steps of
;;   O((n / m) log m) with binary-search tangents; squaring m per round
;;   telescopes to O(n log h). The old file ran a plain O(n log n)
;;   monotone chain despite its name.
;; Space: O(n).
;; Purity: core Clojure only, no host interop. Exact integer arithmetic
;;   for all orientation tests; manual tokenizer and integer parser.
;; Robustness: blank stdin prints nothing; malformed input prints a
;;   message to stderr and returns normally (exit 0 -- a nonzero exit
;;   needs host interop, which this port forbids). A final containment
;;   check falls back to the monotone chain if the march ever disagrees
;;   with it, so output is always the true hull.
(ns computational-geometry.convex-hull.chan-hull
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

(defn cross
  "Twice the signed area of triangle a-b-c; exact integer math."
  [[ax ay] [bx by] [cx cy]]
  (-' (*' (-' bx ax) (-' cy ay)) (*' (-' by ay) (-' cx ax))))

(defn dsq
  "Squared distance; exact integer math."
  [[ax ay] [bx by]]
  (+' (*' (-' ax bx) (-' ax bx)) (*' (-' ay by) (-' ay by))))

(defn graham-scan
  "Monotone chain hull in counter-clockwise order."
  [pts]
  (let [pts (vec (distinct (sort pts)))]
    (if (<= (count pts) 2)
      pts
      (let [grow (fn [hull p]
                   (loop [h hull]
                     (if (and (>= (count h) 2)
                              (<= (cross (nth h (- (count h) 2)) (peek h) p) 0))
                       (recur (pop h))
                       (conj h p))))
            lower (reduce grow [] pts)
            upper (reduce grow [] (reverse pts))]
        (into (pop lower) (pop upper))))))

(defn better?
  "True when c1 is a better march target from p than c2: smaller polar
  angle wins, collinear ties go to the farther point."
  [p c1 c2]
  (let [o (cross p c1 c2)]
    (or (pos? o)
        (and (zero? o) (> (dsq p c1) (dsq p c2))))))

(defn tangent-edge
  "Edge index of the rightmost tangent from p to the counter-clockwise
  strictly convex polygon h (3 or more points), or nil when p leaves no
  edge strictly left of it. Edges with p strictly left form one rotated
  true-block; the values at edges 0 and m-1 reveal the rotation, and the
  rightmost vertex splits the wrapped cases into linear halves, so every
  case is a plain binary search in O(log m)."
  [h p]
  (let [m (count h)
        left? (fn [i]
                (pos? (cross (nth h i) (nth h (rem (inc i) m)) p)))
        xat (fn [i] (nth (nth h i) 0))
        first-true (fn [lo hi]
                     (loop [lo lo hi hi]
                       (if (= lo hi)
                         (when (left? lo) lo)
                         (let [mid (quot (+ lo hi) 2)]
                           (if (left? mid)
                             (recur lo mid)
                             (recur (inc mid) hi))))))
        rmax (loop [lo 0 hi (dec m)]
               (if (= lo hi)
                 lo
                 (let [mid (quot (+ lo hi) 2)]
                   (if (< (xat mid) (xat (inc mid)))
                     (recur (inc mid) hi)
                     (recur lo mid)))))
        l0 (left? 0)
        lm (left? (dec m))]
    (cond
      (and (not l0) lm) (first-true 0 (dec m))
      (and l0 (not lm)) 0
      (and (not l0) (not lm)) (first-true 0 rmax)
      :else (first-true rmax (dec m)))))

(defn tangent-right
  "Rightmost tangent vertex of h seen from p, or nil. A collinear
  neighbor on the same ray wins when farther, matching the chain rule
  that drops intermediate collinear points."
  [h p]
  (let [m (count h)
        t (tangent-edge h p)]
    (when (some? t)
      (let [v (nth h t)
            improve (fn [best w]
                      (if (and (zero? (cross p best w))
                               (> (dsq p w) (dsq p best))
                               (pos? (+' (*' (-' (nth w 0) (nth p 0))
                                              (-' (nth best 0) (nth p 0)))
                                         (*' (-' (nth w 1) (nth p 1))
                                              (-' (nth best 1) (nth p 1))))))
                        w
                        best))]
        (-> v
            (improve (nth h (rem (+ t m -1) m)))
            (as-> w (improve w (nth h (rem (inc t) m)))))))))

(defn group-candidates
  "March candidates contributed by one group: the tangent vertex for
  foreign groups, both hull neighbors for the group holding p."
  [h p own-idx]
  (let [m (count h)]
    (cond
      (some? own-idx)
      (cond (= m 1) []
            (= m 2) [(nth h (rem (inc own-idx) m))]
            :else [(nth h (rem (+ own-idx m -1) m))
                   (nth h (rem (inc own-idx) m))])
      (= m 1) [(nth h 0)]
      (= m 2) (let [[a b] h] (if (better? p a b) [a] [b]))
      :else (let [t (tangent-right h p)]
              (if (some? t) [t] [])))))

(defn hull-contains-all?
  "True when every point lies left of or on each hull edge."
  [hull pts]
  (let [h (count hull)]
    (cond (< h 3)
          (if (< h 2)
            true
            (every? #(zero? (cross (nth hull 0) (nth hull 1) %)) pts))
          :else
          (every? (fn [i]
                    (let [a (nth hull i)
                          b (nth hull (rem (inc i) h))]
                      (every? #(>= (cross a b %) 0) pts)))
                  (range h)))))

(defn try-march
  "Gift-wrap with per-group tangents, capped at m edges; nil on failure."
  [hulls group-of index-of p0 m]
  (loop [hull [p0] at p0 seen #{p0}]
    (if (>= (count hull) (inc m))
      nil
      (let [q (reduce (fn [best [gi h]]
                        (let [own (when (= gi (group-of at))
                                    (index-of at))
                              cands (group-candidates h at own)]
                          (reduce (fn [b c]
                                    (cond (nil? b) c
                                          (better? at c b) c
                                          :else b))
                                  best cands)))
                      nil (map-indexed vector hulls))]
        (cond (nil? q) nil
              (= q p0) hull
              (contains? seen q) nil
              :else (recur (conj hull q) q (conj seen q)))))))

(defn chan-hull
  "Hull vertices by Chan's method, verified against containment."
  [pts]
  (let [pts (vec (distinct (sort pts)))
        n (count pts)]
    (if (<= n 2)
      pts
      (loop [m (min n 4)]
        (let [groups (vec (map vec (partition-all m pts)))
              hulls (vec (map graham-scan groups))]
          (if (= (count groups) 1)
            (first hulls)
            (let [group-of (into {} (for [[gi g] (map-indexed vector groups)
                                           p g]
                                       [p gi]))
                  index-of (into {} (for [h hulls
                                           [i p] (map-indexed vector h)]
                                      [p i]))
                  res (try-march hulls group-of index-of
                                 (first pts) m)]
              (if (and (some? res) (hull-contains-all? res pts))
                res
                (let [m2 (if (> m 3037000499) n (* m m))]
                  (if (>= m2 n)
                    (graham-scan pts)
                    (recur m2)))))))))))

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
                           [(parse-long-token xs) (parse-long-token ys)]))]
            (if (some (fn [[a b]] (or (nil? a) (nil? b))) raw)
              (die "error: expected integer coordinate pairs after the count")
              (let [hull (chan-hull raw)
                    hull (graham-scan hull)
                    min-idx (if (empty? hull)
                              0
                              (first (reduce (fn [a b]
                                               (if (neg? (compare (second b)
                                                                  (second a)))
                                                 b a))
                                             (map-indexed vector hull))))
                    rotated (vec (concat (subvec hull min-idx)
                                         (subvec hull 0 min-idx)))]
                (println (count rotated))
                (doseq [[x y] rotated]
                  (println (str x " " y)))))))))))

(-main)
