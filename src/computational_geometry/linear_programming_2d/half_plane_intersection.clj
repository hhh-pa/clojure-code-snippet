;; 2D Half-Plane Intersection Area (de Berg Ch 4.4).
;;
;; Time: O(N log N) via polar-angle sort and monotone deque intersection.
;; Space: O(N) for sorted half-planes and the active deque.
;; Purity: core Clojure only, zero host interop.
;; Robustness: blank stdin prints nothing; malformed input prints an error
;;   message to *err* and exits normally.
(ns computational-geometry.linear-programming-2d.half-plane-intersection
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

(defn abs-num
  [x]
  (if (neg? x) (-' x) x))

(defn gcd
  [a b]
  (loop [a (abs-num a) b (abs-num b)]
    (if (zero? b)
      a
      (recur b (rem a b)))))

(defn round-double
  [x]
  (if (neg? x)
    (- (long (+ (- x) 0.5)))
    (long (+ x 0.5))))

(defn half-of
  [[dx dy _]]
  (if (or (pos? dy) (and (zero? dy) (pos? dx))) 0 1))

(defn cmp-hp
  [[dx1 dy1 c1 :as h1] [dx2 dy2 c2 :as h2]]
  (let [p1 (half-of h1)
        p2 (half-of h2)]
    (if (not= p1 p2)
      (compare p1 p2)
      (let [cr (-' (*' dx1 dy2) (*' dy1 dx2))]
        (cond
          (pos? cr) -1
          (neg? cr) 1
          :else (compare c2 c1))))))

(defn outside?
  [[dx dy c] [px py]]
  (< (+ (* (- (double dy)) px) (* (double dx) py))
     (- (double c) 1.0e-9)))

(defn intersect
  [[dx1 dy1 c1] [dx2 dy2 c2]]
  (let [det (double (-' (*' dx1 dy2) (*' dy1 dx2)))
        c1d (double c1)
        c2d (double c2)
        x (/ (- (* c1d (double dx2)) (* (double dx1) c2d)) det)
        y (/ (- (* c1d (double dy2)) (* (double dy1) c2d)) det)]
    [x y]))

(defn dist-sq
  [[x1 y1] [x2 y2]]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (+ (* dx dx) (* dy dy))))

(defn solve-hpi
  [raw-quads]
  (let [normed (vec (for [[x1 y1 x2 y2] raw-quads
                          :let [dx (- x2 x1)
                                dy (- y2 y1)
                                g (gcd dx dy)]
                          :when (pos? g)]
                      (let [dx (quot dx g)
                            dy (quot dy g)]
                        [dx dy (-' (*' dx y1) (*' dy x1))])))
        sorted (vec (sort cmp-hp normed))
        hps (reduce (fn [acc [dx dy _ :as h]]
                      (if (or (empty? acc)
                              (let [[pdx pdy _] (peek acc)]
                                (or (not= pdx dx) (not= pdy dy))))
                        (conj acc h)
                        acc))
                    [] sorted)]
    (loop [rem-hps hps
           dq []]
      (if (seq rem-hps)
        (let [h (first rem-hps)
              dq1 (loop [d dq]
                    (if (and (>= (count d) 2)
                             (outside? h (intersect (nth d (- (count d) 2)) (peek d))))
                      (recur (pop d))
                      d))
              dq2 (loop [d dq1]
                    (if (and (>= (count d) 2)
                             (outside? h (intersect (nth d 0) (nth d 1))))
                      (recur (subvec d 1))
                      d))]
          (if (and (seq dq2)
                   (let [[ldx ldy _] (peek dq2)
                         [hdx hdy _] h]
                     (zero? (-' (*' ldx hdy) (*' ldy hdx)))))
            [0 0]
            (recur (rest rem-hps) (conj dq2 h))))
        (let [dq3 (loop [d dq]
                    (if (and (>= (count d) 3)
                             (outside? (nth d 0) (intersect (nth d (- (count d) 2)) (peek d))))
                      (recur (pop d))
                      d))
              dq4 (loop [d dq3]
                    (if (and (>= (count d) 3)
                             (outside? (peek d) (intersect (nth d 0) (nth d 1))))
                      (recur (subvec d 1))
                      d))
              m (count dq4)]
          (if (< m 3)
            [0 0]
            (let [dets-ok (every? (fn [i]
                                    (let [[dx1 dy1 _] (nth dq4 i)
                                          [dx2 dy2 _] (nth dq4 (rem (inc i) m))]
                                      (pos? (-' (*' dx1 dy2) (*' dy1 dx2)))))
                                  (range m))]
              (if-not dets-ok
                [0 0]
                (let [pts (vec (for [i (range m)]
                                 (intersect (nth dq4 i) (nth dq4 (rem (inc i) m)))))
                      dedup (reduce (fn [acc pt]
                                      (if (or (empty? acc)
                                              (> (dist-sq (peek acc) pt) 1.0e-14))
                                        (conj acc pt)
                                        acc))
                                    [] pts)
                      verts (if (and (>= (count dedup) 2)
                                     (<= (dist-sq (first dedup) (peek dedup)) 1.0e-14))
                              (pop dedup)
                              dedup)
                      v (count verts)]
                  (if (< v 3)
                    [0 0]
                    (let [area2 (reduce (fn [acc i]
                                          (let [[ax ay] (nth verts i)
                                                [bx by] (nth verts (rem (inc i) v))]
                                            (+ acc (- (* ax by) (* ay bx)))))
                                        0.0 (range v))
                          area (* 0.5 (if (neg? area2) (- area2) area2))]
                      (if (<= area 1.0e-9)
                        [0 0]
                        [v (round-double (* area 1000.0))]))))))))))))

(defn die
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
          (die "error: first token must be an integer half-plane count")

          (<= n 0)
          (die "error: half-plane count must be positive")

          (< (count (rest tokens)) (* 4 n))
          (die "error: insufficient coordinate tokens")

          :else
          (let [coords (take (* 4 n) (rest tokens))
                quads (vec (for [[a b c d] (partition 4 coords)]
                             [(parse-long-token a) (parse-long-token b)
                              (parse-long-token c) (parse-long-token d)]))]
            (if (some (fn [q] (some nil? q)) quads)
              (die "error: malformed coordinate token")
              (let [[v area-out] (solve-hpi quads)]
                (println (str v " " area-out))))))))))

(-main)
