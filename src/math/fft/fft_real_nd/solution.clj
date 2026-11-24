;; FFT of real data in two and three dimensions by packing into a
;; half-length complex FFT plus split/recombine with a Nyquist
;; side array.
;; Reference: Press et al., Numerical Recipes 3e, Section 12.6 (rlft3,
;; vendoring the fourn engine from 12.5).
;; Documented decisions: the inverse returns (n1*n2*n3/2) times the
;; inverse; dims must be powers of 2 with nn1,nn2 >= 1 and nn3 >= 2
;; (nn3 = 1 would index out of bounds in the book's inverse path),
;; plus fourn's own ntot >= 2 check -- anything else reports zero
;; values with flag 1; theta uses isign*(6.28.../nn3) as written;
;; the truncated literal 6.28318530717959 is used verbatim.
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

(defn ispow2 [n] (and (>= n 1) (= (bit-and n (dec n)) 0)))

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

(defn rlft3 [^doubles data ^doubles speq isign nn1 nn2 nn3]
  (if (or (not (ispow2 nn1)) (not (ispow2 nn2)) (< nn3 2)
          (not= (bit-and nn3 (dec nn3)) 0))
    false
    (let [c1 0.5
          c2 (* -0.5 (double isign))
          theta (* (double isign) (/ 6.28318530717959 (double nn3)))
          wtemp (Math/sin (* 0.5 theta))
          wpr (* (* -2.0 wtemp) wtemp)
          wpi (Math/sin theta)
          nn [nn1 nn2 (bit-shift-right nn3 1)]
          spq (fn [a b] (aget ^doubles speq (+ (* a (* 2 nn2)) b)))
          setspq (fn [a b v] (aset ^doubles speq (+ (* a (* 2 nn2)) b) (double v)))]
      (when (= isign 1)
        (when-not (fourn data nn isign)
          (throw (ex-info "fourn" {})))
        (loop [i1 0 k1 0]
          (when (< i1 nn1)
            (let [k1 (loop [i2 0 j2 0 k1 k1]
                       (if (< i2 nn2)
                         (do
                           (setspq i1 j2 (aget ^doubles data k1))
                           (setspq i1 (+ j2 1) (aget ^doubles data (+ k1 1)))
                           (recur (inc i2) (+ j2 2) (+ k1 nn3)))
                         k1))]
              (recur (inc i1) k1)))))
      (loop [i1 0]
        (when (< i1 nn1)
          (let [j1 (if (not= i1 0) (- nn1 i1) 0)]
            (loop [i3 0 wr 1.0 wi 0.0]
              (when (<= i3 (bit-shift-right nn3 1))
                (let [k1 (* i1 (* nn2 nn3))
                      k3 (* j1 (* nn2 nn3))]
                  (loop [i2 0 k1 k1]
                    (when (< i2 nn2)
                      (if (= i3 0)
                        (let [j2 (if (not= i2 0) (bit-shift-left (- nn2 i2) 1) 0)
                              h1r (* c1 (+ (aget ^doubles data k1) (spq j1 j2)))
                              h1i (* c1 (- (aget ^doubles data (+ k1 1))
                                            (spq j1 (+ j2 1))))
                              h2i (* c2 (- (aget ^doubles data k1) (spq j1 j2)))
                              h2r (* (- c2) (+ (aget ^doubles data (+ k1 1))
                                                (spq j1 (+ j2 1))))]
                          (aset ^doubles data k1 (+ h1r h2r))
                          (aset ^doubles data (+ k1 1) (+ h1i h2i))
                          (setspq j1 j2 (- h1r h2r))
                          (setspq j1 (+ j2 1) (- h2i h1i)))
                        (let [j2 (if (not= i2 0) (- nn2 i2) 0)
                              j3 (- nn3 i3)
                              k2 (+ k1 i3)
                              k4 (+ k3 (* j2 nn3) j3)
                              h1r (* c1 (+ (aget ^doubles data k2)
                                            (aget ^doubles data k4)))
                              h1i (* c1 (- (aget ^doubles data (+ k2 1))
                                            (aget ^doubles data (+ k4 1))))
                              h2i (* c2 (- (aget ^doubles data k2)
                                            (aget ^doubles data k4)))
                              h2r (* (- c2) (+ (aget ^doubles data (+ k2 1))
                                                (aget ^doubles data (+ k4 1))))]
                          (aset ^doubles data k2
                                (- (+ h1r (* wr h2r)) (* wi h2i)))
                          (aset ^doubles data (+ k2 1)
                                (+ (+ h1i (* wr h2i)) (* wi h2r)))
                          (aset ^doubles data k4
                                (+ (- h1r (* wr h2r)) (* wi h2i)))
                          (aset ^doubles data (+ k4 1)
                                (+ (+ (- h1i) (* wr h2i)) (* wi h2r)))))
                      (recur (inc i2) (+ k1 nn3))))
                  (let [wtemp wr]
                    (recur (+ i3 2)
                           (+ (- (* wtemp wpr) (* wi wpi)) wr)
                           (+ (+ (* wi wpr) (* wtemp wpi)) wi)))))))
          (recur (inc i1))))
      (when (= isign -1)
        (when-not (fourn data nn isign)
          (throw (ex-info "fourn" {}))))
      true)))

(defn clamp2m [v]
  (cond (<= v 0) 0 (> v 2000000) 2000000 :else v))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          is2d (or (= op "RFFT2D") (= op "IRFFT2D"))
          is3d (or (= op "RFFT3D") (= op "IRFFT3D"))]
      (when (or is2d is3d)
        (let [[n1 n2 n3 base] (if is2d
                                [1 (Long/parseLong (nth toks 1))
                                 (Long/parseLong (nth toks 2)) 3]
                                [(Long/parseLong (nth toks 1))
                                 (Long/parseLong (nth toks 2))
                                 (Long/parseLong (nth toks 3)) 4])
              huge (or (> n1 2000000) (> n2 2000000) (> n3 2000000))
              full-data (if (or (<= n1 0) (<= n2 0) (<= n3 0)) 0
                              (if huge 2000001 (* n1 n2 n3)))
              full-speq (if (or (<= n1 0) (<= n2 0)) 0
                              (if huge 2000001 (* n1 2 n2)))
              half3 (if (>= n3 2) (bit-shift-right n3 1) 0)
              ntot (if (or (<= n1 0) (<= n2 0)) 0
                         (if huge 2000001 (* n1 n2 half3)))
              valid (and (ispow2 n1) (ispow2 n2) (>= n3 2)
                         (= (bit-and n3 (dec n3)) 0)
                         (<= full-data 2000000) (<= full-speq 2000000)
                         (>= ntot 2) (= (bit-and ntot (dec ntot)) 0))
              ndata (if valid full-data (clamp2m full-data))
              nspeq (if valid full-speq (clamp2m full-speq))
              inverse (or (= op "IRFFT2D") (= op "IRFFT3D"))
              data (double-array (map #(Double/parseDouble %)
                                      (subvec toks base (+ base ndata))))
              speq (double-array (if inverse
                                   (map #(Double/parseDouble %)
                                        (subvec toks (+ base ndata)
                                                (+ base ndata nspeq)))
                                   (repeat nspeq 0.0)))
              ok (and valid
                      (try (rlft3 data speq (if inverse -1 1) n1 n2 n3)
                           (catch clojure.lang.ExceptionInfo _ false)))]
          (if ok
            (println (str (str/join " "
                                    (concat (map #(fmt6 (canon (aget ^doubles data %)))
                                                 (range ndata))
                                            (when-not inverse
                                              (map #(fmt6 (canon (aget ^doubles speq %)))
                                                   (range nspeq)))))
                          " 0"))
            (let [w (if inverse ndata (+ ndata nspeq))]
              (println (str (str/join " " (concat (repeat w "0.000000") ["1"])))))))))))
