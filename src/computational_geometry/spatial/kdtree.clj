;; Kd-tree for orthogonal range counting and nearest neighbors.
;;
;; Time: O(n log n) build, O(sqrt n + k) expected range counting,
;;   O(log n) expected nearest-neighbor query. The build presorts by x
;;   and by y once and partitions the orders down the recursion, so each
;;   level costs O(n); the old code re-sorted every node (O(n log n) per
;;   level, O(n log squared n) total). The tree shape is identical to the
;;   old one: the root splits the input-stable x-order and deeper levels
;;   refine ties exactly as the old cascade of stable sorts did, hence
;;   query answers -- including nearest-neighbor ties -- match.
;; Space: O(n).
;; Purity: core Clojure only, no host interop. The node table is a
;;   persistent vector; parsing and two-decimal rendering are
;;   hand-written; distances use one square root per query.
;; Robustness: blank stdin prints nothing; malformed input prints a
;;   message to stderr and returns normally (exit 0 -- a nonzero exit
;;   needs host interop, which this port forbids).
(ns computational-geometry.spatial.kdtree
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

(defn build-below
  "Build the subtree for points below the root. x-order/y-order hold the
  same points sorted by [axis, other, index], which reproduces the old
  cascade of stable sorts; stable splits keep the orders, so the level
  costs O(n). Returns [nodes root-index]."
  [x-order y-order depth nodes]
  (let [n (count x-order)]
    (if (zero? n)
      [nodes -1]
      (let [axis (rem depth 2)
            ordered (if (zero? axis) x-order y-order)
            mid (quot n 2)
            mid-pt (nth ordered mid)
            node-idx (count nodes)
            nodes (conj nodes {:pt [(nth mid-pt 0) (nth mid-pt 1)]
                               :left -1 :right -1 :axis axis})
            mid-id (nth mid-pt 2)
            before (into #{} (map #(nth % 2) (subvec (vec ordered) 0 mid)))
            keep-left? (fn [p] (contains? before (nth p 2)))
            x-left (filterv keep-left? x-order)
            x-right (filterv (fn [p]
                               (let [i (nth p 2)]
                                 (and (not (= i mid-id)) (not (contains? before i)))))
                             x-order)
            y-left (filterv keep-left? y-order)
            y-right (filterv (fn [p]
                               (let [i (nth p 2)]
                                 (and (not (= i mid-id)) (not (contains? before i)))))
                             y-order)
            [nodes left] (build-below x-left y-left (inc depth) nodes)
            [nodes right] (build-below x-right y-right (inc depth) nodes)]
        [(assoc nodes node-idx (assoc (nth nodes node-idx)
                                      :left left :right right))
         node-idx]))))

(defn build-kdtree
  "Build the node table; the root splits the input-stable x-order so the
  whole shape matches the old per-level sort."
  [pts]
  (if (empty? pts)
    []
    (let [by-x (vec (sort-by (fn [[x _ i]] [x i]) pts))
          n (count by-x)
          mid (quot n 2)
          mid-pt (nth by-x mid)
          node-idx 0
          nodes [{:pt [(nth mid-pt 0) (nth mid-pt 1)]
                  :left -1 :right -1 :axis 0}]
          before (into #{} (map #(nth % 2) (subvec by-x 0 mid)))
          mid-id (nth mid-pt 2)
          in-left? (fn [p] (contains? before (nth p 2)))
          in-right? (fn [p] (let [i (nth p 2)]
                              (and (not (= i mid-id)) (not (contains? before i)))))
          left-pts (filterv in-left? pts)
          right-pts (filterv in-right? pts)
          x-left (vec (sort-by (fn [[x y i]] [x y i]) left-pts))
          y-left (vec (sort-by (fn [[x y i]] [y x i]) left-pts))
          x-right (vec (sort-by (fn [[x y i]] [x y i]) right-pts))
          y-right (vec (sort-by (fn [[x y i]] [y x i]) right-pts))
          [nodes left] (build-below x-left y-left 1 nodes)
          [nodes right] (build-below x-right y-right 1 nodes)]
      (assoc nodes node-idx (assoc (nth nodes node-idx)
                                   :left left :right right)))))

(defn count-in-range
  "Count points inside the rectangle, with the old pruning."
  [nodes xmin ymin xmax ymax]
  (letfn [(query [u]
            (if (= u -1)
              0
              (let [{:keys [pt left right axis]} (nth nodes u)
                    [x y] pt
                    hit (if (and (>= x xmin) (<= x xmax)
                                 (>= y ymin) (<= y ymax))
                          1 0)]
                (+ hit
                   (if (zero? axis)
                     (+ (if (<= xmin x) (query left) 0)
                        (if (>= xmax x) (query right) 0))
                     (+ (if (<= ymin y) (query left) 0)
                        (if (>= ymax y) (query right) 0)))))))]
    (if (empty? nodes) 0 (query 0))))

(defn find-nearest
  "Nearest point and distance to the query, with the old visit order."
  [nodes qx qy]
  (if (empty? nodes)
    [[0.0 0.0] 1e9]
    (letfn [(query [u best-pt best-dsq]
              (if (= u -1)
                [best-pt best-dsq]
                (let [{:keys [pt left right axis]} (nth nodes u)
                      [x y] pt
                      dx (- x qx)
                      dy (- y qy)
                      dsq (+ (* dx dx) (* dy dy))
                      [best-pt best-dsq] (if (< dsq best-dsq)
                                           [pt dsq]
                                           [best-pt best-dsq])
                      axis-diff (if (zero? axis) (- qx x) (- qy y))
                      first-child (if (<= axis-diff 0.0) left right)
                      second-child (if (<= axis-diff 0.0) right left)
                      [best-pt best-dsq] (query first-child best-pt best-dsq)]
                  (if (< (* axis-diff axis-diff) best-dsq)
                    (query second-child best-pt best-dsq)
                    [best-pt best-dsq]))))]
      (let [[best-pt best-dsq] (query 0 [0.0 0.0] 1e18)]
        [best-pt (sqrt best-dsq)]))))

(defn die
  "Report invalid input on stderr and return normally."
  [msg]
  (binding [*out* *err*]
    (println msg)))

(defn run-queries
  "Answer the query tokens against the built tree. A query against an
  empty tree is invalid: the old code crashed with an out-of-bounds
  error and empty stdout, so report it on stderr and print nothing."
  [nodes tokens num-queries]
  (if (and (empty? nodes) (pos? num-queries) (seq tokens))
    (die "error: query on empty tree")
    (loop [q 0 rem-tokens tokens]
    (when (and (< q num-queries) (seq rem-tokens))
      (let [q-type (first rem-tokens)]
        (if (= q-type "RANGE")
          (if (< (count rem-tokens) 5)
            (die "error: truncated RANGE query")
            (let [x1 (parse-double-token (nth rem-tokens 1))
                  y1 (parse-double-token (nth rem-tokens 2))
                  x2 (parse-double-token (nth rem-tokens 3))
                  y2 (parse-double-token (nth rem-tokens 4))]
              (if (some nil? [x1 y1 x2 y2])
                (die "error: RANGE query needs four numbers")
                (do (println (count-in-range nodes x1 y1 x2 y2))
                    (recur (inc q) (drop 5 rem-tokens))))))
          (if (< (count rem-tokens) 3)
            (die "error: truncated nearest query")
            (let [qx (parse-double-token (nth rem-tokens 1))
                  qy (parse-double-token (nth rem-tokens 2))]
              (if (some nil? [qx qy])
                (die "error: nearest query needs two numbers")
                (let [[[px py] d] (find-nearest nodes qx qy)]
                  (println (str (format-fixed (double px) 2) " "
                                (format-fixed (double py) 2) " "
                                (format-fixed (double d) 2)))
                  (recur (inc q) (drop 3 rem-tokens))))))))))))

(defn -main
  [& _args]
  (let [content (slurp *in*)]
    (when-not (str/blank? content)
      (let [tokens (vec (tokenize content))
            n (parse-long-token (first tokens))]
        (cond
          (nil? n)
          (die "error: first token must be an integer point count")

          (or (> n 4611686018427387903) (< n -4611686018427387904))
          (die "error: point count out of range")

          :else
          (let [want (* 2 n)
                pts-tokens (take want (rest tokens))
                after (vec (drop (inc want) tokens))
                raw (vec (for [[xs ys] (partition 2 pts-tokens)]
                           [(parse-double-token xs) (parse-double-token ys)]))]
            (if (some (fn [[a b]] (or (nil? a) (nil? b))) raw)
              (die "error: expected numeric coordinate pairs after the count")
              (let [pts (vec (map-indexed (fn [i [x y]] [x y i]) raw))
                    tree (build-kdtree pts)]
                (when (seq after)
                  (let [num-queries (parse-long-token (first after))]
                    (if (nil? num-queries)
                      (die "error: expected an integer query count")
                      (run-queries tree (vec (rest after)) num-queries))))))))))))

(-main)
