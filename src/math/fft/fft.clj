;; Polynomial multiply mod 998244353 via NTT in pure code.
;;
;; stdin shape: deg-a deg-b, then a coeffs, then b coeffs.
;; stdout: product degree, then product coeffs on one line.
;; Time is n log n in padded size, space is linear in n. Each
;; butterfly pass reads a persistent vector and builds the next one
;; with a transient, so no host arrays stay in the code.
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

(def ntt-mod
  "NTT prime of the old script."
  998244353)

(def ntt-gen
  "Primitive root used by the old script."
  3)

(defn mod-pow
  "Power by squaring under modulus m."
  [base exp m]
  (loop [b (rem base m)
         e exp
         res 1]
    (if (> e 0)
      (recur (rem (* b b) m)
             (quot e 2)
             (if (== (rem e 2) 1)
               (rem (* res b) m)
               res))
      res)))

(defn mod-inv
  "Modular inverse under prime modulus m."
  [x m]
  (mod-pow x (- m 2) m))

(defn rev-bits
  "Bit reverse of i over width bits."
  [i width]
  (loop [k 0
         v i
         acc 0]
    (if (>= k width)
      acc
      (recur (inc k)
             (bit-shift-right v 1)
             (bit-or (bit-shift-left acc 1) (bit-and v 1))))))

(defn bit-reversed
  "Input vector permuted into bit reversed index order."
  [a width]
  (let [n (count a)]
    (mapv (fn [i] (nth a (rev-bits i width))) (range n))))

(defn ntt-stage
  "One butterfly pass. Reads arr, writes a fresh vector of same size."
  [arr len wlen half m]
  (let [n (count arr)]
    (loop [i 0
           t (transient arr)]
      (if (>= i n)
        (persistent! t)
        (recur (+ i len)
               (loop [j 0
                      w 1
                      t t]
                 (if (>= j half)
                   t
                   (let [u (nth arr (+ i j))
                         v (rem (* (nth arr (+ i j half)) w) m)
                         sum (+ u v)
                         diff (- u v)]
                     (recur (inc j)
                            (rem (* w wlen) m)
                            (-> t
                                (assoc! (+ i j)
                                        (if (< sum m) sum (- sum m)))
                                (assoc! (+ i j half)
                                        (if (>= diff 0) diff (+ diff m)))))))))))))

(defn ntt
  "Forward or inverse NTT over a padded coeff vector."
  [a invert]
  (let [n (count a)
        width (loop [w 0
                     p 1]
                (if (>= p n) w (recur (inc w) (* p 2))))]
    (if (<= n 1)
      (if invert
        (let [n-inv (mod-inv n ntt-mod)]
          (mapv (fn [x] (rem (* x n-inv) ntt-mod)) a))
        a)
      (loop [len 2
             arr (bit-reversed a width)]
        (if (> len n)
          (if invert
            (let [n-inv (mod-inv n ntt-mod)]
              (mapv (fn [x] (rem (* x n-inv) ntt-mod)) arr))
            arr)
          (let [w (mod-pow ntt-gen (quot (- ntt-mod 1) len) ntt-mod)
                wlen (if invert (mod-inv w ntt-mod) w)]
            (recur (* len 2)
                   (ntt-stage arr len wlen (quot len 2) ntt-mod))))))))

(defn poly-mul
  "Product of two padded coeff vectors under the NTT prime."
  [a b]
  (let [fa (ntt a false)
        fb (ntt b false)
        prod (mapv (fn [x y] (rem (* x y) ntt-mod)) fa fb)]
    (ntt prod true)))

(defn -main
  "Read stdin, multiply the two polys, print degree and coeffs."
  []
  (let [input (slurp *in*)]
    (when-not (str/blank? input)
      (let [toks (vec (str/split (str/trim input) #"\s+"))]
        (cond
          (< (count toks) 2)
          (fail "Error: need deg-a and deg-b")

          (not (every? int-token? (take 2 toks)))
          (fail "Error: deg-a and deg-b must be integers")

          :else
          (let [deg-a (parse-int (nth toks 0))
                deg-b (parse-int (nth toks 1))
                need (+' 2 (+' deg-a 1) (+' deg-b 1))]
            (cond
              (or (neg? deg-a) (neg? deg-b))
              (fail "Error: degrees must be non-negative")

              (< (count toks) need)
              (fail "Error: not enough coefficients for given degrees")

              (not (every? int-token? (take need (drop 2 toks))))
              (fail "Error: coefficients must be integers")

              :else
              (let [n (loop [sz 1]
                        (if (< sz (+' deg-a deg-b 1))
                          (recur (* sz 2))
                          sz))
                    get-a (take (+' deg-a 1) (drop 2 toks))
                    get-b (take (+' deg-b 1) (drop (+' 3 deg-a) toks))
                    pad (fn [cs] (vec (concat (map parse-int cs)
                                              (repeat (- n (count cs)) 0))))
                    prod (poly-mul (pad get-a) (pad get-b))
                    final-deg (+' deg-a deg-b)]
                (println final-deg)
                (println (str/join " " (map str (take (+' final-deg 1) prod))))))))))))

(-main)
