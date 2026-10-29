(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          t (Integer/parseInt (nth toks 0))]
      (letfn [(mod-mul [a b m]
                (mod (* (bigint a) (bigint b)) (bigint m)))
              (mod-pow [base exp m]
                (loop [res (bigint 1)
                       b (mod (bigint base) (bigint m))
                       e (bigint exp)]
                  (if (zero? e)
                    res
                    (recur (if (odd? e) (mod-mul res b m) res)
                           (mod-mul b b m)
                           (quot e 2)))))
              (gcd [a b]
                (loop [x (bigint a)
                       y (bigint b)]
                  (if (zero? y)
                    x
                    (recur y (mod x y)))))
              (miller-rabin? [n]
                (let [n (bigint n)]
                  (cond
                    (< n 2) false
                    (or (== n 2) (== n 3)) true
                    (even? n) false
                    :else
                    (let [ds (loop [d (dec n)
                                    s 0]
                               (if (even? d)
                                 (recur (quot d 2) (inc s))
                                 [d s]))
                          d (nth ds 0)
                          s (nth ds 1)
                          nm1 (dec n)
                          bases [2 3 5 7 11 13 17 19 23 29 31 37]]
                      (loop [bs bases]
                        (if (empty? bs)
                          true
                          (let [a (bigint (first bs))]
                            (if (<= n a)
                              true
                              (let [x (mod-pow a d n)]
                                (if (or (== x 1) (== x nm1))
                                  (recur (rest bs))
                                  (let [witness (loop [r 1
                                                       y x]
                                                  (if (>= r s)
                                                    false
                                                    (let [y2 (mod-mul y y n)]
                                                      (if (== y2 nm1)
                                                        true
                                                        (recur (inc r) y2)))))]
                                    (if witness
                                      (recur (rest bs))
                                      false))))))))))))
              (pollard-rho [n]
                (let [n (bigint n)]
                  (if (even? n)
                    (bigint 2)
                    (let [f (fn [v c]
                              (mod (+ (mod-mul v v n) (bigint c)) n))]
                      (loop [x (bigint 2)
                             y (bigint 2)
                             d (bigint 1)
                             c (bigint 1)]
                        (if (== d 1)
                          (let [nx (f x c)
                                ny (f (f y c) c)
                                diff (if (> nx ny) (- nx ny) (- ny nx))
                                nd (gcd diff n)]
                            (if (== nd n)
                              (recur (bigint 2) (bigint 2) (bigint 1) (inc c))
                              (recur nx ny nd c)))
                          d))))))
              (factorize [n]
                (let [n (bigint n)]
                  (cond
                    (== n 1) []
                    (miller-rabin? n) [n]
                    :else (let [d (pollard-rho n)]
                            (vec (concat (factorize d)
                                         (factorize (quot n d))))))))]
        (loop [i 0
               ti 1]
          (when (< i t)
            (let [n (bigint (nth toks ti))]
              (if (miller-rabin? n)
                (println "PRIME")
                (let [factors (sort (factorize n))]
                  (println (str "COMPOSITE " (clojure.string/join " " (map str factors)))))))
            (recur (inc i) (inc ti))))))))
