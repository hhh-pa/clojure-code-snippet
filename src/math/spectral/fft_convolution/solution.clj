;; Cyclic convolution, deconvolution and correlation of real data
;; via the packed real FFT.
;; Reference: Press et al., Numerical Recipes 3e, Sections 13.2-13.3
;; (convlv/correl, vendoring realft+four1 from 12.2-12.3).
;; Documented decisions: n must be a power of 2 >= 4 (realft needs
;; n/2 >= 2); m must be odd with 1 <= m <= n (the book would
;; silently misbehave or index out of bounds outside this);
;; deconvolution at an exact response zero, like any other book
;; throw, reports zero values with flag 1; SQR(x) is x*x.
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

(defn convlv [^doubles data ^doubles respns isign ^doubles ans]
  (let [n (alength ^doubles data)
        m (alength ^doubles respns)]
    (if (or (< m 1) (> m n) (= (bit-and m 1) 0))
      false
      (let [temp (double-array n)]
        (aset ^doubles temp 0 (aget ^doubles respns 0))
        (loop [i 1]
          (when (< i (quot (+ m 1) 2))
            (aset ^doubles temp i (aget ^doubles respns i))
            (aset ^doubles temp (- n i) (aget ^doubles respns (- m i)))
            (recur (inc i))))
        (loop [i (quot (+ m 1) 2)]
          (when (< i (- n (quot (- m 1) 2)))
            (aset ^doubles temp i 0.0)
            (recur (inc i))))
        (dotimes [i n]
          (aset ^doubles ans i (aget ^doubles data i)))
        (when-not (realft ans 1) (throw (ex-info "realft" {})))
        (when-not (realft temp 1) (throw (ex-info "realft" {})))
        (let [no2 (double (bit-shift-right n 1))]
          (cond
            (= isign 1)
            (do
              (loop [i 2]
                (when (< i n)
                  (let [tmp (aget ^doubles ans i)]
                    (aset ^doubles ans i
                          (/ (- (* (aget ^doubles ans i) (aget ^doubles temp i))
                                 (* (aget ^doubles ans (+ i 1))
                                    (aget ^doubles temp (+ i 1))))
                              no2))
                    (aset ^doubles ans (+ i 1)
                          (/ (+ (* (aget ^doubles ans (+ i 1))
                                    (aget ^doubles temp i))
                                 (* tmp (aget ^doubles temp (+ i 1))))
                              no2)))
                  (recur (+ i 2))))
              (aset ^doubles ans 0
                    (/ (* (aget ^doubles ans 0) (aget ^doubles temp 0)) no2))
              (aset ^doubles ans 1
                    (/ (* (aget ^doubles ans 1) (aget ^doubles temp 1)) no2)))
            (= isign -1)
            (do
              (loop [i 2]
                (when (< i n)
                  (let [mag2 (+ (* (aget ^doubles temp i) (aget ^doubles temp i))
                                 (* (aget ^doubles temp (+ i 1))
                                    (aget ^doubles temp (+ i 1))))]
                    (when (= mag2 0.0)
                      (throw (ex-info "respzero" {})))
                    (let [tmp (aget ^doubles ans i)]
                      (aset ^doubles ans i
                            (/ (/ (+ (* (aget ^doubles ans i)
                                          (aget ^doubles temp i))
                                       (* (aget ^doubles ans (+ i 1))
                                          (aget ^doubles temp (+ i 1))))
                                    mag2)
                                no2))
                      (aset ^doubles ans (+ i 1)
                            (/ (/ (- (* (aget ^doubles ans (+ i 1))
                                          (aget ^doubles temp i))
                                       (* tmp (aget ^doubles temp (+ i 1))))
                                    mag2)
                                no2))))
                  (recur (+ i 2))))
              (when (or (= (aget ^doubles temp 0) 0.0)
                        (= (aget ^doubles temp 1) 0.0))
                (throw (ex-info "respzero" {})))
              (aset ^doubles ans 0
                    (/ (/ (aget ^doubles ans 0) (aget ^doubles temp 0)) no2))
              (aset ^doubles ans 1
                    (/ (/ (aget ^doubles ans 1) (aget ^doubles temp 1)) no2)))
            :else (throw (ex-info "isign" {}))))
        (when-not (realft ans -1) (throw (ex-info "realft" {})))
        true))))

(defn correl [^doubles data1 ^doubles data2 ^doubles ans]
  (let [n (alength ^doubles data1)
        temp (double-array n)]
    (dotimes [i n]
      (aset ^doubles ans i (aget ^doubles data1 i))
      (aset ^doubles temp i (aget ^doubles data2 i)))
    (when-not (realft ans 1) (throw (ex-info "realft" {})))
    (when-not (realft temp 1) (throw (ex-info "realft" {})))
    (let [no2 (double (bit-shift-right n 1))]
      (loop [i 2]
        (when (< i n)
          (let [tmp (aget ^doubles ans i)]
            (aset ^doubles ans i
                  (/ (+ (* (aget ^doubles ans i) (aget ^doubles temp i))
                         (* (aget ^doubles ans (+ i 1))
                            (aget ^doubles temp (+ i 1))))
                      no2))
            (aset ^doubles ans (+ i 1)
                  (/ (- (* (aget ^doubles ans (+ i 1)) (aget ^doubles temp i))
                         (* tmp (aget ^doubles temp (+ i 1))))
                      no2)))
          (recur (+ i 2))))
      (aset ^doubles ans 0
            (/ (* (aget ^doubles ans 0) (aget ^doubles temp 0)) no2))
      (aset ^doubles ans 1
            (/ (* (aget ^doubles ans 1) (aget ^doubles temp 1)) no2)))
    (when-not (realft ans -1) (throw (ex-info "realft" {})))
    true))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (cond
        (or (= op "CONV") (= op "DECONV"))
        (let [n (Long/parseLong (nth toks 1))
              m (Long/parseLong (nth toks 2))
              nn (max 0 n)
              mm (max 0 m)
              data (double-array (map #(Double/parseDouble %)
                                      (subvec toks 3 (+ 3 nn))))
              respns (double-array (map #(Double/parseDouble %)
                                        (subvec toks (+ 3 nn) (+ 3 nn mm))))
              ans (double-array nn)
              ok (try (convlv data respns (if (= op "CONV") 1 -1) ans)
                      (catch clojure.lang.ExceptionInfo _ false))]
          (if ok
            (println (str (str/join " " (map #(fmt6 (canon (aget ^doubles ans %)))
                                              (range nn)))
                          " 0"))
            (println (str (str/join " " (concat (repeat nn "0.000000") ["1"]))))))
        (= op "CORR")
        (let [n (Long/parseLong (nth toks 1))
              nn (max 0 n)
              d1 (double-array (map #(Double/parseDouble %)
                                    (subvec toks 2 (+ 2 nn))))
              d2 (double-array (map #(Double/parseDouble %)
                                    (subvec toks (+ 2 nn) (+ 2 nn nn))))
              ans (double-array nn)
              ok (try (correl d1 d2 ans)
                      (catch clojure.lang.ExceptionInfo _ false))]
          (if ok
            (println (str (str/join " " (map #(fmt6 (canon (aget ^doubles ans %)))
                                              (range nn)))
                          " 0"))
            (println (str (str/join " " (concat (repeat nn "0.000000") ["1"]))))))))))
