(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          nt (count toks)
          n (Integer/parseInt (nth toks 0))
          pts (java.util.ArrayList.)
          _ (loop [i 0]
              (when (and (< i n) (< (+ 2 (* i 2)) nt))
                (.add pts [(Integer/parseInt (nth toks (+ 1 (* i 2))))
                            (Integer/parseInt (nth toks (+ 2 (* i 2))))])
                (recur (inc i))))
          sorted (vec (sort (seq pts)))
          uniq (loop [i 0, acc (transient [])]
                 (if (>= i (count sorted))
                   (persistent! acc)
                   (let [p (nth sorted i)]
                     (if (and (> i 0) (= p (nth sorted (dec i))))
                       (recur (inc i) acc)
                       (recur (inc i) (conj! acc p))))))
          cross (fn [o a b]
                  (- (* (- (nth a 0) (nth o 0)) (- (nth b 1) (nth o 1)))
                     (* (- (nth a 1) (nth o 1)) (- (nth b 0) (nth o 0)))))
          dist (fn [a b]
                 (let [dx (double (- (nth a 0) (nth b 0)))
                       dy (double (- (nth a 1) (nth b 1)))]
                   (Math/sqrt (+ (* dx dx) (* dy dy)))))
          chain (fn [points]
                  (let [st (java.util.ArrayList.)]
                    (doseq [p points]
                      (loop []
                        (when (>= (.size st) 2)
                          (let [sz (.size st)]
                            (when (<= (cross (.get st (- sz 2)) (.get st (- sz 1)) p) 0)
                              (.remove st (int (dec sz)))
                              (recur)))))
                      (.add st p))
                    (when (> (.size st) 0) (.remove st (int (dec (.size st)))))
                    (vec (seq st))))]
      (let [fmt1 (fn [x]
                     ;; C-correct rounding of the exact binary value (printf/iostream
                     ;; semantics); clojure format uses HALF_UP on the short repr.
                     (str (.toString (.setScale (BigDecimal. (double x)) 1
                                                java.math.RoundingMode/HALF_EVEN))
                          "\n"))]
        (cond
          (< (count uniq) 2)
          (print "0.0\n")
          (= (count uniq) 2)
          (print (fmt1 (* 2.0 (dist (nth uniq 0) (nth uniq 1)))))
          :else
          (let [lower (chain uniq)
                upper (chain (vec (reverse uniq)))
                hull (vec (concat lower upper))
                m (count hull)
                per (loop [i 0, acc 0.0]
                      (if (>= i m)
                        acc
                        (recur (inc i) (+ acc (dist (nth hull i) (nth hull (mod (inc i) m)))))))]
            (print (fmt1 per))))))))
