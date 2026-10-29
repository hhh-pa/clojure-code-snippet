;; Strassen matrix multiply in pure code.
;;
;; stdin shape: n, then two n by n matrices row by row.
;; stdout: product rows, one row per line.
;; Pads to a power of two, splits into quadrants, runs seven half
;; size products, then joins. Time is n to the log2 of 7, extra space
;; is quadratic. Small blocks use direct multiply. Exact integer math
;; keeps every block size choice bit identical in output.
;; Blank stdin gives no output. Bad input gives a stderr note and the
;; script returns normally, so exit stays zero with no host exit call.

(require '[clojure.string :as str])

(def digit-values
  "Digit char to int value."
  (zipmap "0123456789" (range)))

(defn parse-int
  "Parse a signed decimal token with exact math."
  [tok]
  (let [neg (= \- (first tok))
        body (if neg (rest tok) (seq tok))]
    (let [v (reduce (fn [acc ch] (+' (*' acc 10) (digit-values ch)))
                    0
                    body)]
      (if neg (-' v) v))))

(defn int-token?
  "True when tok is an optionally signed run of decimal digits."
  [tok]
  (boolean
   (and (seq tok)
        (let [body (if (= \- (first tok)) (rest tok) (seq tok))]
          (and (seq body)
               (every? digit-values body))))))

(defn fail
  "Print message to stderr, then return normally with exit zero."
  [msg]
  (binding [*out* *err*]
    (println msg)))

(def base-case
  "Block size where direct multiply takes over from recursion."
  32)

(defn madd
  "Element wise matrix sum."
  [a b]
  (mapv (fn [ra rb] (mapv +' ra rb)) a b))

(defn msub
  "Element wise matrix difference."
  [a b]
  (mapv (fn [ra rb] (mapv -' ra rb)) a b))

(defn dot-row-col
  "Dot product of row ra with column j of b."
  [ra b j sz]
  (loop [k 0
         acc 0]
    (if (>= k sz)
      acc
      (recur (inc k) (+' acc (*' (nth ra k) (nth (nth b k) j)))))))

(defn standard-mult
  "Direct cubic multiply for small blocks."
  [a b]
  (let [sz (count a)]
    (mapv (fn [ra]
            (mapv (fn [j] (dot-row-col ra b j sz))
                  (range sz)))
          a)))

(defn split4
  "Four quadrant views of a square matrix."
  [m]
  (let [sz (count m)
        k (quot sz 2)
        top (subvec m 0 k)
        bot (subvec m k sz)]
    [[(mapv (fn [r] (subvec r 0 k)) top)
      (mapv (fn [r] (subvec r k sz)) top)]
     [(mapv (fn [r] (subvec r 0 k)) bot)
      (mapv (fn [r] (subvec r k sz)) bot)]]))

(defn stitch-rows
  "Join left and right row blocks side by side."
  [left right]
  (mapv (fn [rl rr] (vec (concat rl rr))) left right))

(defn combine4
  "Join four quadrants into one square matrix."
  [c11 c12 c21 c22]
  (vec (concat (stitch-rows c11 c12)
               (stitch-rows c21 c22))))

(defn strassen-mult
  "Strassen recursion with a direct base case."
  [a b]
  (let [sz (count a)]
    (if (<= sz base-case)
      (standard-mult a b)
      (let [[[a11 a12] [a21 a22]] (split4 a)
            [[b11 b12] [b21 b22]] (split4 b)
            m1 (strassen-mult (madd a11 a22) (madd b11 b22))
            m2 (strassen-mult (madd a21 a22) b11)
            m3 (strassen-mult a11 (msub b12 b22))
            m4 (strassen-mult a22 (msub b21 b11))
            m5 (strassen-mult (madd a11 a12) b22)
            m6 (strassen-mult (msub a21 a11) (madd b11 b12))
            m7 (strassen-mult (msub a12 a22) (madd b21 b22))]
        (combine4 (madd (msub (madd m1 m4) m5) m7)
                  (madd m3 m5)
                  (madd m2 m4)
                  (madd (madd (msub m1 m2) m3) m6))))))

(defn pad-to
  "Pad square flat rows of size n up to square size m with zeros."
  [flat n m]
  (mapv (fn [i]
          (mapv (fn [j]
                  (if (and (< i n) (< j n))
                    (nth (nth flat i) j)
                    0))
                (range m)))
        (range m)))

(defn -main
  "Read stdin, multiply the pair, print product rows."
  []
  (let [input (slurp *in*)]
    (when-not (str/blank? input)
      (let [toks (vec (str/split (str/trim input) #"\s+"))]
        (cond
          (not (int-token? (first toks)))
          (fail "Error: first token must be integer n")

          :else
          (let [n (parse-int (first toks))
                need (+' 1 (*' 2 n n))]
            (cond
              (neg? n)
              (fail "Error: n must be non-negative")

              (< (count toks) need)
              (fail "Error: input too short for n by n pair")

              (not (every? int-token? (take need (rest toks))))
              (fail "Error: entries must be integers")

              (zero? n)
              nil

              :else
              (let [nums (mapv parse-int (take (-' need 1) (rest toks)))
                    half (* n n)
                    a-flat (mapv (fn [i]
                                   (subvec nums (* i n) (+ (* i n) n)))
                                 (range n))
                    b-flat (mapv (fn [i]
                                   (subvec nums
                                           (+ half (* i n))
                                           (+ half (* i n) n)))
                                 (range n))
                    m (loop [p 1]
                        (if (< p n) (recur (* p 2)) p))
                    c (strassen-mult (pad-to a-flat n m)
                                     (pad-to b-flat n m))]
                (doseq [i (range n)]
                  (println (str/join " " (map str (take n (nth c i))))))))))))))

(-main)
