;; Welzl's Smallest Enclosing Disc MiniDisc (de Berg Ch 4.7).
;;
;; Time: O(N) expected (O(N^3) worst case on fixed order).
;; Space: O(N) to store deduplicated points.
;; Purity: core Clojure only, zero host interop.
;; Robustness: blank stdin prints nothing; malformed input prints an error
;;   message to *err* and exits normally.
(ns computational-geometry.smallest-enclosing-disc.welzl-minidisc
  (:require [clojure.string :as str]))

(def digit-values
  "Digit character to numeric value."
  (zipmap "0123456789" (range)))

(defn whitespace?
  "True for ASCII whitespace separating input tokens."
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
  "Parse a signed 64-bit integer token; nil when malformed or out of range."
  [tok]
  (when (some? tok)
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
                  (if want-neg v (- v)))))))))))

(defn round-double
  "Round finite double x to nearest integer (half away from zero)."
  [x]
  (if (neg? x)
    (- (long (+ (- x) 0.5)))
    (long (+ x 0.5))))

(defn dist-sq
  "Squared Euclidean distance between two 2D points."
  [x1 y1 x2 y2]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (+ (* dx dx) (* dy dy))))

(defn in-disc?
  "True when point [px py] lies inside or on disc [cx cy r2]."
  [[cx cy r2] [px py]]
  (<= (dist-sq cx cy (double px) (double py)) (+ r2 1.0e-9)))

(defn disc-from-2
  "Smallest enclosing disc with diameter a-b."
  [[ax ay] [bx by]]
  (let [cx (* 0.5 (double (+ ax bx)))
        cy (* 0.5 (double (+ ay by)))
        dx (double (- bx ax))
        dy (double (- by ay))]
    [cx cy (* 0.25 (+ (* dx dx) (* dy dy)))]))

(defn disc-from-3
  "Circumdisc of points a, b, c (falls back to diameter disc when collinear)."
  [[ax ay :as a] [bx by :as b] [cx cy :as c]]
  (let [bxi (- bx ax)
        byi (- by ay)
        cxi (- cx ax)
        cyi (- cy ay)
        det (- (*' bxi cyi) (*' byi cxi))]
    (if (zero? det)
      (let [d1 (disc-from-2 a b)
            d2 (disc-from-2 a c)
            d3 (disc-from-2 b c)]
        (reduce (fn [best cur] (if (> (nth cur 2) (nth best 2)) cur best))
                d1 [d2 d3]))
      (let [bxd (double bxi)
            byd (double byi)
            cxd (double cxi)
            cyd (double cyi)
            b2 (+ (* bxd bxd) (* byd byd))
            c2 (+ (* cxd cxd) (* cyd cyd))
            d (* 2.0 (double det))
            ux (/ (- (* cyd b2) (* byd c2)) d)
            uy (/ (- (* bxd c2) (* cxd b2)) d)]
        [(+ (double ax) ux) (+ (double ay) uy) (+ (* ux ux) (* uy uy))]))))

(defn mini-disc-with-2-points
  "Smallest disc enclosing pts[0..limit-1] with q1 and q2 on its boundary."
  [pts limit q1 q2]
  (loop [k 0
         d (disc-from-2 q1 q2)]
    (if (= k limit)
      d
      (let [pk (nth pts k)]
        (recur (inc k)
               (if (in-disc? d pk)
                 d
                 (disc-from-3 q1 q2 pk)))))))

(defn mini-disc-with-point
  "Smallest disc enclosing pts[0..limit-1] with q on its boundary."
  [pts limit q]
  (loop [j 1
         d (disc-from-2 (nth pts 0) q)]
    (if (= j limit)
      d
      (let [pj (nth pts j)]
        (recur (inc j)
               (if (in-disc? d pj)
                 d
                 (mini-disc-with-2-points pts j pj q)))))))

(defn welzl-mini-disc
  "Smallest disc enclosing raw-pts."
  [raw-pts]
  (let [pts (vec (distinct (sort raw-pts)))
        u (count pts)]
    (cond
      (zero? u) [0.0 0.0 0.0]
      (= u 1) [(double (first (first pts))) (double (second (first pts))) 0.0]
      :else
      (loop [i 2
             d (disc-from-2 (nth pts 0) (nth pts 1))]
        (if (= i u)
          d
          (let [pi (nth pts i)]
            (recur (inc i)
                   (if (in-disc? d pi)
                     d
                     (mini-disc-with-point pts i pi)))))))))

(defn die
  "Print error message to *err* and return nil."
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

          (<= n 0)
          (die "error: point count must be positive")

          (< (count (rest tokens)) (* 2 n))
          (die "error: insufficient coordinate tokens")

          :else
          (let [coords (take (* 2 n) (rest tokens))
                pts (vec (for [[xs ys] (partition 2 coords)]
                           [(parse-long-token xs) (parse-long-token ys)]))]
            (if (some (fn [[x y]] (or (nil? x) (nil? y))) pts)
              (die "error: malformed coordinate token")
              (let [[cx cy r2] (welzl-mini-disc pts)]
                (println (str (round-double (* cx 1000.0)) " "
                              (round-double (* cy 1000.0)) " "
                              (round-double (* r2 1000.0))))))))))))

(-main)
