;; Generalized Chinese Remainder Theorem over a list of congruences.
;;
;; stdin shape: k then k pairs of r m, meaning x = r (mod m).
;; stdout: merged pair R M with x = R (mod M), else no solution.
;; Time is linear in k times Euclid log cost, extra space is constant.
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

(defn ext-gcd
  "Iterative extended Euclid giving [g s t] with g a gcd of inputs."
  [a b]
  (loop [old-r a
         r b
         old-s 1
         s 0
         old-t 0
         t 1]
    (if (zero? r)
      [old-r old-s old-t]
      (let [q (quot old-r r)]
        (recur r
               (-' old-r (*' q r))
               s
               (-' old-s (*' q s))
               t
               (-' old-t (*' q t)))))))

(defn merge-two
  "Merge x = r1 (mod m1) with x = r2 (mod m2). Gives [r m] or nil."
  [r1 m1 r2 m2]
  (let [[g p _q] (ext-gcd m1 m2)
        diff (-' r2 r1)]
    (when (zero? (rem diff g))
      (let [m2r (quot m2 g)
            factor (rem (quot diff g) m2r)
            k1 (rem (*' (rem p m2r) factor) m2r)
            k1 (if (neg? k1) (+' k1 m2r) k1)
            new-m (*' m1 m2r)
            new-r (rem (+' r1 (*' m1 k1)) new-m)
            new-r (if (neg? new-r) (+' new-r new-m) new-r)]
        [new-r new-m]))))

(defn solve-crt
  "Fold merge-two over pairs. Gives [r m] or nil when in conflict."
  [pairs]
  (reduce (fn [acc pr]
            (let [[r1 m1] acc
                  [r2 m2] pr
                  merged (merge-two r1 m1 r2 m2)]
              (if (nil? merged)
                (reduced nil)
                merged)))
          (first pairs)
          (rest pairs)))

(defn -main
  "Read stdin, solve the system, print the merged pair."
  []
  (let [input (slurp *in*)]
    (when-not (str/blank? input)
      (let [toks (vec (str/split (str/trim input) #"\s+"))]
        (cond
          (not (int-token? (first toks)))
          (fail "Error: first token must be integer k")

          :else
          (let [k (parse-int (first toks))
                rest-toks (rest toks)]
            (cond
              (<= k 0)
              (fail "Error: k must be a positive integer")

              (< (count rest-toks) (*' 2 k))
              (fail "Error: expected k pairs of r m after k")

              (not (every? int-token? (take (*' 2 k) rest-toks)))
              (fail "Error: r and m tokens must be integers")

              :else
              (let [pairs (->> (take (*' 2 k) rest-toks)
                               (map parse-int)
                               (partition 2)
                               (map vec)
                               (vec))]
                (if (some (fn [[_r m]] (zero? m)) pairs)
                  (fail "Error: moduli must be nonzero")
                  (let [res (solve-crt pairs)]
                    (if (nil? res)
                      (println "NO SOLUTION")
                      (let [[r m] res]
                        (println (str r) (str m))))))))))))))

(-main)
