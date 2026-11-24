;; N-dimensional complex FFT (forward and inverse) by successive
;; one-dimensional Danielson-Lanczos passes with per-dimension
;; bit-reversal.
;; Reference: Press et al., Numerical Recipes 3e, Section 12.5 (fourn).
;; Documented decisions: validity is tested on the product of the
;; lengths only (ntot >= 2 a power of 2), exactly as the book does --
;; for positive dims this is equivalent to testing each dim, and
;; length-1 dims are faithful no-ops; the inverse is NOT normalized
;; (returns ntot times the inverse); theta uses the book's integer
;; division ifp2/ip1 and left-assoc isign*6.28.../k; the truncated
;; literal 6.28318530717959 is used verbatim. Robustness: ntot is
;; capped at 10^6 (beyond that, or non-positive dims, the flag-1
;; path runs with output width clamped to 2M values).
;; No online-judge problem found -- fixtures are self-generated
;; (numpy oracle), no header link.
(require '[clojure.string :as str])

(def LIM 1000000)

(defn fmt6 [v]
  (let [neg (neg? v)
        x (if neg (- v) v)
        ip (long x)
        fp (long (+ (* (- x ip) 1000000.0) 0.5))
        [ip fp] (if (= fp 1000000) [(inc ip) 0] [ip fp])]
    (str (when neg "-") ip "." (subs (str (+ 1000000 fp)) 1))))

(defn absv [v] (if (neg? v) (- v) v))

(defn canon [v] (if (< (absv v) 5e-7) 0.0 v))

(defn fourn [^doubles data nn isign]
  (let [ndim (count nn)
        ntot (reduce * 1 nn)]
    (if (or (< ntot 2) (not= (bit-and ntot (dec ntot)) 0))
      false
      (do
        (loop [idim (dec ndim) nprev 1]
          (when (>= idim 0)
            (let [n (nth nn idim)
                  nrem (quot ntot (* n nprev))
                  ip1 (bit-shift-left nprev 1)
                  ip2 (* ip1 n)
                  ip3 (* ip2 nrem)]
              (loop [i2 0 i2rev 0]
                (when (< i2 ip2)
                  (when (< i2 i2rev)
                    (loop [i1 i2]
                      (when (< i1 (- (+ i2 ip1) 1))
                        (loop [i3 i1]
                          (when (< i3 ip3)
                            (let [i3rev (- (+ i2rev i3) i2)
                                  t (aget ^doubles data i3)]
                              (aset ^doubles data i3 (aget ^doubles data i3rev))
                              (aset ^doubles data i3rev t)
                              (let [t (aget ^doubles data (+ i3 1))]
                                (aset ^doubles data (+ i3 1)
                                      (aget ^doubles data (+ i3rev 1)))
                                (aset ^doubles data (+ i3rev 1) t)))
                            (recur (+ i3 ip2))))
                        (recur (+ i1 2)))))
                  (let [[ibit i2rev] (loop [ibit (bit-shift-right ip2 1)
                                            i2rev i2rev]
                                       (if (and (>= ibit ip1) (> (+ i2rev 1) ibit))
                                         (recur (bit-shift-right ibit 1) (- i2rev ibit))
                                         [ibit i2rev]))]
                    (recur (+ i2 ip1) (+ i2rev ibit)))))
              (loop [ifp1 ip1]
                (when (< ifp1 ip2)
                  (let [ifp2 (bit-shift-left ifp1 1)
                        theta (/ (* (double isign) 6.28318530717959)
                                 (double (quot ifp2 ip1)))
                        wtemp (Math/sin (* 0.5 theta))
                        wpr (* (* -2.0 wtemp) wtemp)
                        wpi (Math/sin theta)]
                    (loop [i3 0 wr 1.0 wi 0.0]
                      (when (< i3 ifp1)
                        (loop [i1 i3]
                          (when (< i1 (- (+ i3 ip1) 1))
                            (loop [i2 i1]
                              (when (< i2 ip3)
                                (let [k1 i2
                                      k2 (+ k1 ifp1)
                                      tempr (- (* wr (aget ^doubles data k2))
                                               (* wi (aget ^doubles data (+ k2 1))))
                                      tempi (+ (* wr (aget ^doubles data (+ k2 1)))
                                               (* wi (aget ^doubles data k2)))]
                                  (aset ^doubles data k2
                                        (- (aget ^doubles data k1) tempr))
                                  (aset ^doubles data (+ k2 1)
                                        (- (aget ^doubles data (+ k1 1)) tempi))
                                  (aset ^doubles data k1
                                        (+ (aget ^doubles data k1) tempr))
                                  (aset ^doubles data (+ k1 1)
                                        (+ (aget ^doubles data (+ k1 1)) tempi)))
                                (recur (+ i2 ifp2))))
                            (recur (+ i1 2))))
                        (let [wtemp wr]
                          (recur (+ i3 ip1)
                                 (+ (- (* wtemp wpr) (* wi wpi)) wr)
                                 (+ (+ (* wi wpr) (* wtemp wpi)) wi)))))
                  (recur ifp2))))
              (recur (dec idim) (* nprev n)))))
        true))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          ndim (Long/parseLong (nth toks 1))]
      (when (or (= op "FFTND") (= op "IFFTND"))
        (let [ndim-us (max 0 ndim)
              nn (vec (map #(Long/parseLong %)
                           (subvec toks 2 (+ 2 ndim-us))))
              ntot (if (<= ndim 0)
                     1
                     (loop [xs nn acc 1]
                       (if (empty? xs)
                         acc
                         (let [d (first xs)]
                           (if (or (<= d 0) (> acc LIM) (> d LIM))
                             (if (<= d 0) 0 (+ LIM 1))
                             (let [p (* acc d)]
                               (if (> p LIM) (+ LIM 1) (recur (rest xs) p))))))))
              valid (and (>= ntot 2) (<= ntot LIM)
                         (= (bit-and ntot (dec ntot)) 0))
              nvals (if valid (* 2 ntot)
                        (if (<= ntot 0) 0 (min (* 2 ntot) (* 2 LIM))))
              base (+ 2 ndim-us)
              data (double-array (map #(Double/parseDouble %)
                                      (subvec toks base (+ base nvals))))
              isign (if (= op "FFTND") 1 -1)]
          (if (and valid (fourn data nn isign))
            (println (str (str/join " " (map #(fmt6 (canon (aget ^doubles data %)))
                                              (range nvals)))
                          " 0"))
            (println (str (str/join " " (concat (repeat nvals "0.000000") ["1"]))))))))))
