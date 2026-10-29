(let [input (slurp *in*)]
  (when (not (clojure.string/blank? input))
    (let [toks (.split (.trim input) "\\s+")
          nt (alength toks)
          n (Integer/parseInt (aget toks 0))
          ;; Number of complete (x, y) pairs actually present.
          m (min n (quot (dec nt) 2))
          ;; Pack each point into one long key: signed x in the high
          ;; half and y biased by 2^31 in the low half, so signed key
          ;; order is lexicographic (x, then y); equal points share a key.
          keys (long-array m)]
      (loop [i 0]
        (when (< i m)
          (let [x (Integer/parseInt (aget toks (+ 1 (* i 2))))
                y (Integer/parseInt (aget toks (+ 2 (* i 2))))]
            (aset keys i (bit-or (bit-shift-left (long x) 32)
                                 (+ (long y) 2147483648)))
            (recur (inc i)))))
      (java.util.Arrays/sort keys)
      ;; Unpack the distinct points into parallel coordinate arrays.
      (let [xs (long-array m)
            ys (long-array m)
            u (loop [i 0, cnt 0]
                (if (>= i m)
                  cnt
                  (let [k (aget keys i)]
                    (if (and (pos? i) (== k (aget keys (dec i))))
                      (recur (inc i) cnt)
                      (do (aset xs cnt (bit-shift-right k 32))
                          (aset ys cnt (long (- (bit-and k 4294967295) 2147483648)))
                          (recur (inc i) (inc cnt)))))))]
        (letfn [(x-at [^long i] (aget xs i))
                (y-at [^long i] (aget ys i))
                (cross [^long o ^long a ^long b]
                  (- (* (- (x-at a) (x-at o)) (- (y-at b) (y-at o)))
                     (* (- (y-at a) (y-at o)) (- (x-at b) (x-at o)))))
                (dist [^long a ^long b]
                  (let [dx (double (- (x-at a) (x-at b)))
                        dy (double (- (y-at a) (y-at b)))]
                    (Math/sqrt (+ (* dx dx) (* dy dy)))))
                ;; Monotone chain over indices lo..hi (step +-1); returns
                ;; the chain without its last point, like a hull scan.
                (chain [^long lo ^long hi ^long step]
                  (let [st (int-array (max 1 u))
                        sz (loop [i lo, sz 0]
                             (if (== i hi)
                               sz
                               (let [sz (loop [sz sz]
                                          (if (and (>= sz 2)
                                                   (<= (cross (aget st (- sz 2))
                                                              (aget st (- sz 1))
                                                              i)
                                                       0))
                                            (recur (dec sz))
                                            sz))]
                                 (aset st sz (int i))
                                 (recur (+ i step) (inc sz)))))]
                    ;; Drop the last point of the chain.
                    (let [len (if (pos? sz) (dec sz) 0)
                          out (int-array (max 0 len))]
                      (System/arraycopy st 0 out 0 len)
                      out)))]
          (let [fmt1 (fn [x]
                       ;; C-correct rounding of the exact binary value (printf/iostream
                       ;; semantics); clojure format uses HALF_UP on the short repr.
                       (str (.toString (.setScale (BigDecimal. (double x)) 1
                                                  java.math.RoundingMode/HALF_EVEN))
                            "\n"))]
            (cond
              (< u 2)
              (print "0.0\n")
              (== u 2)
              (print (fmt1 (* 2.0 (dist 0 1))))
              :else
              (let [^ints lower (chain 0 u 1)
                    ^ints upper (chain (dec u) -1 -1)
                    hull (int-array (+ (alength lower) (alength upper)))
                    _ (do (System/arraycopy lower 0 hull 0 (alength lower))
                          (System/arraycopy upper 0 hull (alength lower) (alength upper)))
                    m2 (alength hull)
                    per (loop [i 0, acc 0.0]
                          (if (>= i m2)
                            acc
                            (recur (inc i) (+ acc (dist (aget hull i)
                                                        (aget hull (mod (inc i) m2)))))))]
                (print (fmt1 per))))))))))
