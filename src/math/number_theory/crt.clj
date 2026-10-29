(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          k (Integer/parseInt (nth toks 0))]
      (letfn [(ext-gcd [a b]
                (if (zero? b)
                  [a 1 0]
                  (let [[d x1 y1] (ext-gcd b (rem a b))]
                    [d y1 (- x1 (* y1 (quot a b)))])))]
        (let [r0 (Long/parseLong (nth toks 1))
              m0 (Long/parseLong (nth toks 2))]
          (loop [i 1
                 r1 r0
                 m1 m0]
            (if (>= i k)
              (println (str r1) (str m1))
              (let [r2 (Long/parseLong (nth toks (+ 1 (* 2 i))))
                    m2 (Long/parseLong (nth toks (+ 2 (* 2 i))))
                    [g p _] (ext-gcd m1 m2)
                    diff (-' r2 r1)]
                (if (not (zero? (rem diff g)))
                  (println "NO SOLUTION")
                  (let [m2r (quot m2 g)
                        factor (rem (quot diff g) m2r)
                        k1 (rem (*' (rem p m2r) factor) m2r)
                        k1 (if (neg? k1) (+ k1 m2r) k1)
                        new-m (*' m1 m2r)
                        new-r (rem (+' r1 (*' m1 k1)) new-m)
                        new-r (if (neg? new-r) (+ new-r new-m) new-r)]
                    (recur (inc i) new-r new-m)))))))))))
