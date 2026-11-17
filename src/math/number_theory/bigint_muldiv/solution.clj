;; Bigint mul/divmod on decimal digit arrays (LSB first):
;; grade-school multiplication with a Karatsuba fast path plus long
;; division. No native bigint.
(require '[clojure.string :as str])

;; Grade-school below this many digits; Karatsuba above it.
(def karat-threshold 16)

(defn strip-vec [d]
  (loop [d d]
    (if (and (> (count d) 1) (zero? (peek d)))
      (recur (pop d))
      d)))

(defn cmp-vec [a b]
  (cond (not= (count a) (count b)) (if (< (count a) (count b)) -1 1)
        :else
        (loop [i (dec (count a))]
          (cond (< i 0) 0
                (not= (nth a i) (nth b i)) (if (< (nth a i) (nth b i)) -1 1)
                :else (recur (dec i))))))

(defn add-vec [a b]
  (loop [i 0
         carry 0
         out []]
    (if (>= i (max (count a) (count b)))
      (strip-vec (if (pos? carry) (conj out carry) out))
      (let [v (+ (get a i 0) (get b i 0) carry)]
        (recur (inc i) (quot v 10) (conj out (rem v 10)))))))

(defn sub-vec [a b]
  (loop [i 0
         borrow 0
         out []]
    (if (>= i (count a))
      (strip-vec out)
      (let [v (- (nth a i) (get b i 0) borrow)]
        (if (neg? v)
          (recur (inc i) 1 (conj out (+ v 10)))
          (recur (inc i) 0 (conj out v)))))))

(defn mul-school [a b]
  (let [out (loop [i 0
                   out (vec (repeat (+ (count a) (count b)) 0))]
              (if (>= i (count a))
                out
                (recur (inc i)
                       (loop [j 0
                              out out]
                         (if (>= j (count b))
                           out
                           (recur (inc j)
                                  (assoc out (+ i j) (+ (nth out (+ i j))
                                                         (* (nth a i) (nth b j))))))))))]
    (loop [i 0
           carry 0
           out out]
      (if (>= i (count out))
        (strip-vec (loop [carry carry
                           out out]
                      (if (zero? carry)
                        out
                        (recur (quot carry 10) (conj out (rem carry 10))))))
        (let [v (+ (nth out i) carry)]
          (recur (inc i) (quot v 10) (assoc out i (rem v 10))))))))

(defn mul-vec [a b]
  (let [[a b] (if (< (count a) (count b)) [b a] [a b])]
    (if (< (count b) karat-threshold)
      (mul-school a b)
      (let [m (quot (count a) 2)
            a0 (subvec a 0 m)
            a1 (subvec a m)
            b0 (subvec b 0 (min m (count b)))
            b1 (if (> (count b) m) (subvec b m) [0])
            z0 (mul-vec a0 b0)
            z2 (mul-vec a1 b1)
            z1 (sub-vec (sub-vec (mul-vec (add-vec a0 a1) (add-vec b0 b1)) z0) z2)]
        (strip-vec (add-vec (add-vec (vec (concat (repeat (* 2 m) 0) z2))
                                          (vec (concat (repeat m 0) z1)))
                                 z0))))))

(defn trial-digit [b rem]
  (loop [q 9]
    (if (or (zero? q) (<= (cmp-vec (mul-school b [q]) rem) 0))
      q
      (recur (dec q)))))

(defn divmod-vec [a b]
  (if (neg? (cmp-vec a b))
    [[0] a]
    (loop [i (dec (count a))
           quot (vec (repeat (count a) 0))
           rem []]
      (if (neg? i)
        [(strip-vec quot) (strip-vec rem)]
        (let [rem (strip-vec (vec (cons (nth a i) rem)))
              q (trial-digit b rem)]
          (recur (dec i) (assoc quot i q) (sub-vec rem (mul-school b [q]))))))))

(defn parse-vec [s]
  (strip-vec (mapv #(- (int %) (int \0)) (reverse s))))

(defn show-vec [d]
  (apply str (map #(char (+ (int \0) %)) (reverse d))))

(defn -main
  "Read OP and two decimals; print the product or quotient/remainder."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 3)
      (let [a (parse-vec (nth toks 1))
            b (parse-vec (nth toks 2))]
        (if (= (nth toks 0) "MUL")
          (print (show-vec (mul-vec a b)))
          (let [[q r] (divmod-vec a b)]
            (print (str (show-vec q) " " (show-vec r)))))))))

(-main (slurp *in*))
