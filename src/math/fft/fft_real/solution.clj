;; FFT of real-valued data by packing into half-length complex FFT
;; (forward) with packed positive-frequency output, and its inverse.
;; Reference: Press et al., Numerical Recipes 3e, Section 12.3 (realft,
;; vendoring the four1 engine from 12.2).
;; Documented decisions: the inverse is NOT normalized (returns n/2
;; times the inverse -- "result must be multiplied by 2/n"); n must be
;; a power of 2 >= 4 (four1 needs n/2 >= 2), anything else reports zero
;; values with flag 1 (the book throws for n/2 invalid; odd n >= 5
;; would silently misbehave there, so it is rejected here);
;; the pi literal 3.141592653589793238 is used verbatim.
;; No online-judge problem found -- fixtures are self-generated
;; (numpy oracle), no header link.
(require '[clojure.string :as str])

(defn fmt6 [v]
  (let [neg (neg? v)
        x (if neg (- v) v)
        ip (long x)
        fp (long (+ (* (- x ip) 1000000.0) 0.5))
        [ip fp] (if (= fp 1000000) [(inc ip) 0] [ip fp])]
    (str (when neg "-") ip "." (subs (str (+ 1000000 fp)) 1))))

(defn absv [v] (if (neg? v) (- v) v))

(defn canon [v] (if (< (absv v) 5e-7) 0.0 v))

(defn four1 [^doubles data n isign]
  (if (or (< n 2) (not= (bit-and n (dec n)) 0))
    false
    (let [nn (bit-shift-left n 1)]
      (loop [i 1 j 1]
        (when (< i nn)
          (when (> j i)
            (let [t (aget ^doubles data (dec j))]
              (aset ^doubles data (dec j) (aget ^doubles data (dec i)))
              (aset ^doubles data (dec i) t))
            (let [t (aget ^doubles data j)]
              (aset ^doubles data j (aget ^doubles data i))
              (aset ^doubles data i t)))
          (let [[m j] (loop [m n j j]
                        (if (and (>= m 2) (> j m))
                          (recur (bit-shift-right m 1) (- j m))
                          [m j]))]
            (recur (+ i 2) (+ j m)))))
      (loop [mmax 2]
        (when (> nn mmax)
          (let [istep (bit-shift-left mmax 1)
                theta (* (double isign) (/ 6.28318530717959 (double mmax)))
                wtemp (Math/sin (* 0.5 theta))
                wpr (* (* -2.0 wtemp) wtemp)
                wpi (Math/sin theta)]
            (loop [m 1 wr 1.0 wi 0.0]
              (when (< m mmax)
                (loop [i m]
                  (when (<= i nn)
                    (let [j (+ i mmax)
                          tempr (- (* wr (aget ^doubles data (dec j)))
                                   (* wi (aget ^doubles data j)))
                          tempi (+ (* wr (aget ^doubles data j))
                                   (* wi (aget ^doubles data (dec j))))]
                      (aset ^doubles data (dec j)
                            (- (aget ^doubles data (dec i)) tempr))
                      (aset ^doubles data j
                            (- (aget ^doubles data i) tempi))
                      (aset ^doubles data (dec i)
                            (+ (aget ^doubles data (dec i)) tempr))
                      (aset ^doubles data i
                            (+ (aget ^doubles data i) tempi)))
                    (recur (+ i istep))))
                (let [wtemp wr]
                  (recur (+ m 2)
                         (+ (- (* wtemp wpr) (* wi wpi)) wr)
                         (+ (+ (* wi wpr) (* wtemp wpi)) wi)))))
            (recur istep))))
      true)))

(defn realft [^doubles data isign]
  (let [n (alength ^doubles data)]
    (if (or (< n 4) (not= (bit-and n (dec n)) 0))
      false
      (let [c1 0.5
            half (bit-shift-right n 1)
            c2 (if (= isign 1) -0.5 0.5)
            theta (* (if (= isign 1) 1.0 -1.0)
                     (/ 3.141592653589793238 (double half)))]
        (when (= isign 1)
          (four1 data half 1))
        (let [wtemp (Math/sin (* 0.5 theta))
              wpr (* (* -2.0 wtemp) wtemp)
              wpi (Math/sin theta)]
          (loop [i 1 wr (+ 1.0 wpr) wi wpi]
            (when (< i (bit-shift-right n 2))
              (let [i1 (+ i i)
                    i2 (+ 1 i1)
                    i3 (- n i1)
                    i4 (+ 1 i3)
                    h1r (* c1 (+ (aget ^doubles data i1) (aget ^doubles data i3)))
                    h1i (* c1 (- (aget ^doubles data i2) (aget ^doubles data i4)))
                    h2r (* (- c2) (+ (aget ^doubles data i2) (aget ^doubles data i4)))
                    h2i (* c2 (- (aget ^doubles data i1) (aget ^doubles data i3)))]
                (aset ^doubles data i1
                      (- (+ h1r (* wr h2r)) (* wi h2i)))
                (aset ^doubles data i2
                      (+ (+ h1i (* wr h2i)) (* wi h2r)))
                (aset ^doubles data i3
                      (+ (- h1r (* wr h2r)) (* wi h2i)))
                (aset ^doubles data i4
                      (+ (+ (- h1i) (* wr h2i)) (* wi h2r)))
                (let [wtemp wr]
                  (recur (inc i)
                         (+ (- (* wtemp wpr) (* wi wpi)) wr)
                         (+ (+ (* wi wpr) (* wtemp wpi)) wi)))))))
        (if (= isign 1)
          (let [h1r (aget ^doubles data 0)]
            (aset ^doubles data 0 (+ h1r (aget ^doubles data 1)))
            (aset ^doubles data 1 (- h1r (aget ^doubles data 1))))
          (let [h1r (aget ^doubles data 0)]
            (aset ^doubles data 0 (* c1 (+ h1r (aget ^doubles data 1))))
            (aset ^doubles data 1 (* c1 (- h1r (aget ^doubles data 1))))
            (four1 data half -1)))
        true))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          n (Long/parseLong (nth toks 1))]
      (when (or (= op "RFFT") (= op "IRFFT"))
        (let [isign (if (= op "RFFT") 1 -1)
              len (max 0 n)
              data (double-array (map #(Double/parseDouble %)
                                      (subvec toks 2 (+ 2 len))))]
          (if (realft data isign)
            (println (str (str/join " " (map #(fmt6 (canon (aget ^doubles data %)))
                                              (range len)))
                          " 0"))
            (println (str (str/join " " (concat (repeat len "0.000000") ["1"]))))))))))
