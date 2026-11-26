;; Fast sine and cosine transforms via the real FFT plus
;; auxiliary-array packing and odd/even unfolding.
;; Reference: Press et al., Numerical Recipes 3e, Section 12.4
;; (sinft/cosft1/cosft2, vendoring realft+four1 from 12.2-12.3).
;; Documented decisions: sinft/cosft1 are self-inverse up to a
;; factor (applying twice gives n/2 times the input -- "output
;; should be multiplied by 2/n" for inverse use); cosft2 inverse
;; likewise returns n/2 times the inverse; sizes must be powers
;; of 2 >= 4 (realft needs n/2 >= 2), anything else reports zero
;; values with flag 1; sinft forces y[0] = 0 (f_0 is identically
;; 0), ignoring input y[0]; the pi literal 3.141592653589793238
;; is used verbatim.
;; No online-judge problem found -- fixtures are self-generated
;; (numpy oracle), no header link.
(require '[clojure.string :as str])

(def PI 3.141592653589793238)

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
                     (/ PI (double half)))]
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

(defn sinft [^doubles y]
  (let [n (alength ^doubles y)]
    (if (or (< n 4) (not= (bit-and n (dec n)) 0))
      false
      (let [theta (/ PI (double n))
            wtemp (Math/sin (* 0.5 theta))
            wpr (* (* -2.0 wtemp) wtemp)
            wpi (Math/sin theta)]
        (aset ^doubles y 0 0.0)
        (loop [j 1 wr 1.0 wi 0.0]
          (when (< j (+ (bit-shift-right n 1) 1))
            (let [wtemp wr
                  wr (+ (- (* wtemp wpr) (* wi wpi)) wr)
                  wi (+ (+ (* wi wpr) (* wtemp wpi)) wi)
                  y1 (* wi (+ (aget ^doubles y j) (aget ^doubles y (- n j))))
                  y2 (* 0.5 (- (aget ^doubles y j) (aget ^doubles y (- n j))))]
              (aset ^doubles y j (+ y1 y2))
              (aset ^doubles y (- n j) (- y1 y2))
              (recur (inc j) wr wi))))
        (realft y 1)
        (aset ^doubles y 0 (* (aget ^doubles y 0) 0.5))
        (aset ^doubles y 1 0.0)
        (loop [j 0 sum 0.0]
          (when (< j (dec n))
            (let [sum (+ sum (aget ^doubles y j))]
              (aset ^doubles y j (aget ^doubles y (+ j 1)))
              (aset ^doubles y (+ j 1) sum)
              (recur (+ j 2) sum))))
        true))))

(defn cosft1 [^doubles y]
  (let [n (dec (alength ^doubles y))]
    (if (or (< n 4) (not= (bit-and n (dec n)) 0))
      false
      (let [theta (/ PI (double n))
            wtemp (Math/sin (* 0.5 theta))
            wpr (* (* -2.0 wtemp) wtemp)
            wpi (Math/sin theta)
            yy (double-array n)]
        (aset ^doubles yy 0 (* 0.5 (+ (aget ^doubles y 0) (aget ^doubles y n))))
        (let [sum (loop [j 1 wr 1.0 wi 0.0
                         sum (* 0.5 (- (aget ^doubles y 0) (aget ^doubles y n)))]
                    (if (< j (/ n 2))
                      (let [wtemp wr
                            wr (+ (- (* wtemp wpr) (* wi wpi)) wr)
                            wi (+ (+ (* wi wpr) (* wtemp wpi)) wi)
                            y1 (* 0.5 (+ (aget ^doubles y j)
                                         (aget ^doubles y (- n j))))
                            y2 (- (aget ^doubles y j) (aget ^doubles y (- n j)))]
                        (aset ^doubles yy j (- y1 (* wi y2)))
                        (aset ^doubles yy (- n j) (+ y1 (* wi y2)))
                        (recur (inc j) wr wi (+ sum (* wr y2))))
                      sum))]
          (aset ^doubles yy (/ n 2) (aget ^doubles y (/ n 2)))
          (realft yy 1)
          (dotimes [j n]
            (aset ^doubles y j (aget ^doubles yy j)))
          (aset ^doubles y n (aget ^doubles y 1))
          (aset ^doubles y 1 sum)
          (loop [j 3 sum sum]
            (when (< j n)
              (let [sum (+ sum (aget ^doubles y j))]
                (aset ^doubles y j sum)
                (recur (+ j 2) sum)))))
        true))))

(defn cosft2 [^doubles y isign]
  (let [n (alength ^doubles y)]
    (if (or (< n 4) (not= (bit-and n (dec n)) 0))
      false
      (let [theta (/ (* 0.5 PI) (double n))
            wpr (* (* -2.0 (Math/sin theta)) (Math/sin theta))
            wpi (Math/sin (* 2.0 theta))]
        (if (= isign 1)
          (do
            (loop [i 0 wr1 (Math/cos theta) wi1 (Math/sin theta)]
              (when (< i (/ n 2))
                (let [y1 (* 0.5 (+ (aget ^doubles y i)
                                    (aget ^doubles y (- (dec n) i))))
                      y2 (* wi1 (- (aget ^doubles y i)
                                    (aget ^doubles y (- (dec n) i))))]
                  (aset ^doubles y i (+ y1 y2))
                  (aset ^doubles y (- (dec n) i) (- y1 y2))
                  (let [wtemp wr1]
                    (recur (inc i)
                           (+ (- (* wtemp wpr) (* wi1 wpi)) wr1)
                           (+ (+ (* wi1 wpr) (* wtemp wpi)) wi1))))))
            (realft y 1)
            (loop [i 2 wr 1.0 wi 0.0]
              (when (< i n)
                (let [wtemp wr
                      wr (+ (- (* wtemp wpr) (* wi wpi)) wr)
                      wi (+ (+ (* wi wpr) (* wtemp wpi)) wi)
                      y1 (- (* (aget ^doubles y i) wr)
                            (* (aget ^doubles y (+ i 1)) wi))
                      y2 (+ (* (aget ^doubles y (+ i 1)) wr)
                            (* (aget ^doubles y i) wi))]
                  (aset ^doubles y i y1)
                  (aset ^doubles y (+ i 1) y2)
                  (recur (+ i 2) wr wi))))
            (loop [i (dec n) sum (* 0.5 (aget ^doubles y 1))]
              (when (> i 0)
                (let [sum1 sum
                      sum (+ sum (aget ^doubles y i))]
                  (aset ^doubles y i sum1)
                  (recur (- i 2) sum)))))
          (do
            (let [ytemp (aget ^doubles y (dec n))]
              (loop [i (dec n)]
                (when (> i 2)
                  (aset ^doubles y i (- (aget ^doubles y (- i 2))
                                         (aget ^doubles y i)))
                  (recur (- i 2))))
              (aset ^doubles y 1 (* 2.0 ytemp)))
            (loop [i 2 wr 1.0 wi 0.0]
              (when (< i n)
                (let [wtemp wr
                      wr (+ (- (* wtemp wpr) (* wi wpi)) wr)
                      wi (+ (+ (* wi wpr) (* wtemp wpi)) wi)
                      y1 (+ (* (aget ^doubles y i) wr)
                            (* (aget ^doubles y (+ i 1)) wi))
                      y2 (- (* (aget ^doubles y (+ i 1)) wr)
                            (* (aget ^doubles y i) wi))]
                  (aset ^doubles y i y1)
                  (aset ^doubles y (+ i 1) y2)
                  (recur (+ i 2) wr wi))))
            (realft y -1)
            (loop [i 0 wr1 (Math/cos theta) wi1 (Math/sin theta)]
              (when (< i (/ n 2))
                (let [y1 (+ (aget ^doubles y i) (aget ^doubles y (- (dec n) i)))
                      y2 (* (/ 0.5 wi1) (- (aget ^doubles y i)
                                            (aget ^doubles y (- (dec n) i))))]
                  (aset ^doubles y i (* 0.5 (+ y1 y2)))
                  (aset ^doubles y (- (dec n) i) (* 0.5 (- y1 y2)))
                  (let [wtemp wr1]
                    (recur (inc i)
                           (+ (- (* wtemp wpr) (* wi1 wpi)) wr1)
                           (+ (+ (* wi1 wpr) (* wtemp wpi)) wi1))))))))
        true))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          n (Long/parseLong (nth toks 1))]
      (cond
        (or (= op "SINFT") (= op "COSFT2F") (= op "COSFT2I"))
        (let [len (max 0 n)
              y (double-array (map #(Double/parseDouble %)
                                   (subvec toks 2 (+ 2 len))))
              ok (cond (= op "SINFT") (sinft y)
                       (= op "COSFT2F") (cosft2 y 1)
                       :else (cosft2 y -1))]
          (if ok
            (println (str (str/join " " (map #(fmt6 (canon (aget ^doubles y %)))
                                              (range len)))
                          " 0"))
            (println (str (str/join " " (concat (repeat len "0.000000") ["1"]))))))
        (= op "COSFT1")
        (let [len (max 0 (+ n 1))
              y (double-array (map #(Double/parseDouble %)
                                   (subvec toks 2 (+ 2 len))))]
          (if (cosft1 y)
            (println (str (str/join " " (map #(fmt6 (canon (aget ^doubles y %)))
                                              (range len)))
                          " 0"))
            (println (str (str/join " " (concat (repeat len "0.000000") ["1"]))))))))))
