;; Power spectrum estimation: windowed periodograms over segments
;; (Spectreg), half-overlapping segments (Spectolap, incl. long-data
;; accordion), and Slepian multitaper (filltable eigensolver).
;; Reference: Press et al., Numerical Recipes 3e, Section 13.5
;; (Spectreg/Spectolap/Slepian, vendoring realft+four1 from 12.2-12.3).
;; Documented decisions: frequencies() is not output (closed form
;; i*0.5/m); unknown window names flag; m must be a power of 2
;; >= 1 (m = 1 flags via realft); SLEPIAN needs m >= 2 (its
;; table build indexes u[3]); Newton breaks identically given
;; identical libm-class inputs; SQR(x) is x*x.
;; No online-judge problem found -- fixtures are self-generated
;; (numpy/scipy oracle), no header link.
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

(defn window-val [wtype hanntab j n m2]
  (cond (= wtype 1) (- 1.0 (absv (- (/ (* 2.0 (double j)) (double (dec n))) 1.0)))
        (= wtype 2) (let [t (- (/ (* 2.0 (double j)) (double (dec n))) 1.0)]
                      (- 1.0 (* t t)))
        (= wtype 3) (when (= n m2) (aget ^doubles hanntab j))
        :else 1.0))

(defn make-hann [m2]
  (let [twopi (* 8.0 (Math/atan 1.0))
        tab (double-array m2)]
    (dotimes [i m2]
      (aset ^doubles tab i (* 0.5 (- 1.0 (Math/cos (/ (* (double i) twopi)
                                                       (double (dec m2))))))))
    tab))

(defn spect-addseg [st data]
  ;; st is an atom holding {:m :m2 :nsum :specsum :wksp :wtype :hanntab}.
  ;; Returns true/false; mutates arrays in place.
  (let [{:keys [m m2 specsum wksp wtype hanntab]} @st
        n (alength ^doubles data)]
    (if (not= n m2)
      false
      (let [sumw (loop [i 0 s 0.0]
                   (if (>= i m2) s
                     (let [w (window-val wtype hanntab i m2 m2)]
                       (if (nil? w)
                         (reduced nil)
                         (do
                           (aset ^doubles wksp i (* w (aget ^doubles data i)))
                           (recur (inc i) (+ s (* w w))))))))]
        (if (nil? sumw)
          false
          (let [fac (/ 2.0 (* sumw (double m2)))]
            (when-not (realft wksp 1)
              (throw (ex-info "realft" {})))
            (aset ^doubles specsum 0 (+ (aget ^doubles specsum 0)
                                        (* (* 0.5 fac)
                                           (* (aget ^doubles wksp 0)
                                              (aget ^doubles wksp 0)))))
            (loop [i 1]
              (when (< i m)
                (aset ^doubles specsum i
                      (+ (aget ^doubles specsum i)
                         (* fac (+ (* (aget ^doubles wksp (* 2 i))
                                       (aget ^doubles wksp (* 2 i)))
                                    (* (aget ^doubles wksp (+ (* 2 i) 1))
                                       (aget ^doubles wksp (+ (* 2 i) 1)))))))
                (recur (inc i))))
            (aset ^doubles specsum m (+ (aget ^doubles specsum m)
                                        (* (* 0.5 fac)
                                           (* (aget ^doubles wksp 1)
                                              (aget ^doubles wksp 1)))))
            (swap! st update :nsum inc)
            true))))))

(defn spect-spectrum [st]
  (let [{:keys [m nsum specsum]} @st]
    (when (> nsum 0)
      (let [out (double-array (inc m))]
        (dotimes [i (inc m)]
          (aset ^doubles out i (/ (aget ^doubles specsum i) (double nsum))))
        out))))

(defn make-spect [m wtype]
  (when (and (>= m 1) (= (bit-and m (dec m)) 0))
    (atom {:m m :m2 (* 2 m) :nsum 0
           :specsum (double-array (inc m))
           :wksp (double-array (* 2 m))
           :wtype wtype
           :hanntab (when (= wtype 3) (make-hann (* 2 m))) })))

(defn ^double ldexp [^double x ^long n] (Math/scalb x (int n)))

(defn slepian-fill [m jres kt]
  ;; Returns dpss table (vector of double-arrays) or nil on invalid params.
  (when (and (>= m 2) (>= jres 1) (< kt (* 2 jres)))
    (let [m2 (* 2 m)
          EPS 1.0e-10
          PI (* 4.0 (Math/atan 1.0))
          dg (double-array m2)
          dgg (double-array m2)
          gam (double-array m2)
          sup (double-array (dec m2))
          sub (double-array (dec m2))
          jd (double jres)
          s0 (Math/sin (/ (* jd PI) (double m2)))
          sw (* 2.0 s0 s0)
          t0 (- (double m2) 1.0)]
      (aset ^doubles dg 0 (* 0.25 (+ (+ (* 2.0 (double m2)) (* sw t0 t0)) -1.0)))
      (dotimes [ii (dec m2)]
        (let [i (inc ii)
              t1 (- (- (double m2) 1.0) (* 2.0 (double i)))]
          (aset ^doubles dg i
                (* 0.25 (+ (* sw t1 t1)
                            (* (double (- (* 2 (- m2 i)) 1))
                               (double (+ (* 2 i) 1))))))
          (aset ^doubles sub (dec i)
                (/ (* (- (double i)) (double (- m2 i))) 2.0))
          (aset ^doubles sup (dec i) (aget ^doubles sub (dec i)))))
      (loop [k 0
             xx (+ (- -0.10859 (/ 0.068762 jd)) (* 1.5692 jd))
             xold 0.0
             dpss []]
        (if (>= k kt)
          dpss
          (let [xx0 (if (zero? k) xx
                        ;; xold from previous k is threaded below
                        xx)]
            ;; Newton iterations (fixed 20, early break on convergence)
            (let [xnew
                  (loop [it 0 xx xx]
                    (if (>= it 20)
                      xx
                      (let [[p d]
                            (loop [j 1
                                   p (- (aget ^doubles dg 0) xx) pp 1.0
                                   d -1.0 dd 0.0]
                              (if (>= j m2)
                                [p d]
                                (let [ppp pp
                                      ddd dd
                                      pp p
                                      dd d
                                      s1 (aget ^doubles sup (dec j))
                                      pnew (- (* pp (- (aget ^doubles dg j) xx))
                                              (* ppp s1 s1))
                                      dnew (- (+ (- pp)
                                                  (* dd (- (aget ^doubles dg j) xx)))
                                              (* ddd s1 s1))
                                      ap (absv pnew)
                                      [pnew pp dnew dd]
                                      (cond (> ap 1.0e30)
                                            [(ldexp pnew -100)
                                             (ldexp pp -100)
                                             (ldexp dnew -100)
                                             (ldexp dd -100)]
                                            (< ap 1.0e-30)
                                            [(ldexp pnew 100)
                                             (ldexp pp 100)
                                             (ldexp dnew 100)
                                             (ldexp dd 100)]
                                            :else [pnew pp dnew dd])]
                                  (recur (inc j) pnew pp dnew dd))))
                            xnew (- xx (/ p d))]
                        (if (< (absv (- xx xnew)) (* EPS (absv xnew)))
                          xnew
                          (recur (inc it) xnew)))))]
              ;; next guess by linear extrapolation (first k uses formula xold)
              (let [xold (if (zero? k)
                           (+ (+ xx0 0.47276 (/ 0.20273 jd)) (- (* 3.1387 jd)))
                           xold)
                    u (double-array m2)]
                (dotimes [i m2]
                  (aset ^doubles dgg i (- (aget ^doubles dg i) xnew)))
                (let [nl (quot m2 3)]
                  (aset ^doubles dgg nl 1.0)
                  (let [ssup (aget ^doubles sup nl)
                        ssub (aget ^doubles sub (dec nl))]
                    (aset ^doubles u 0 0.0)
                    (aset ^doubles sup nl 0.0)
                    (aset ^doubles sub (dec nl) 0.0)
                    (loop [i 1 bet (aget ^doubles dgg 0)]
                      (when (< i m2)
                        (aset ^doubles gam i
                              (/ (aget ^doubles sup (dec i)) bet))
                        (let [bet (- (aget ^doubles dgg i)
                                      (* (aget ^doubles sub (dec i))
                                         (aget ^doubles gam i)))]
                          (aset ^doubles u i
                                (/ (- (if (= i nl) 1.0 0.0)
                                        (* (aget ^doubles sub (dec i))
                                           (aget ^doubles u (dec i))))
                                    bet))
                          (recur (inc i) bet))))
                    (loop [i (- m2 2)]
                      (when (>= i 0)
                        (aset ^doubles u i
                              (- (aget ^doubles u i)
                                 (* (aget ^doubles gam (+ i 1))
                                    (aget ^doubles u (+ i 1)))))
                        (recur (dec i))))
                    (aset ^doubles sup nl ssup)
                    (aset ^doubles sub (dec nl) ssub)
                    (let [s (loop [i 0 sum 0.0]
                              (if (>= i m2) sum
                                (recur (inc i) (+ sum (* (aget ^doubles u i)
                                                          (aget ^doubles u i))))))
                          s (if (> (aget ^doubles u 3) 0.0)
                              (Math/sqrt s)
                              (- (Math/sqrt s)))]
                      (dotimes [i m2]
                        (aset ^doubles u i (/ (aget ^doubles u i) s)))
                      (recur (inc k)
                             (- xnew (- xold xnew))
                             xnew
                             (conj dpss u)))))))))))))

(defn make-slepian [m jres kt]
  (let [dpss (slepian-fill m jres kt)]
    (when dpss
      (let [st (make-spect m -1)]
        (when st
          {:st st :dpss dpss :kt kt})))))

(defn slepian-addseg [sp data]
  (let [{:keys [st dpss kt]} sp
        m2 (:m2 @st)]
    (if (not= (alength ^doubles data) m2)
      false
      (do
        (dotimes [k kt]
          (swap! st assoc :hanntab (nth dpss k) :wtype 3)
          (when-not (spect-addseg st data)
            (throw (ex-info "realft" {}))))
        true))))

(defn emit-spec [^doubles spec]
  (println (str (str/join " " (map #(fmt6 (canon (aget ^doubles spec %)))
                                    (range (alength ^doubles spec))))
                " 0")))

(defn emit-zeros [m]
  (let [w (if (>= m 1) (inc m) 0)]
    (println (str (str/join " " (concat (repeat w "0.000000") ["1"]))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (cond
        (= op "SLEPIAN")
        (let [m (Long/parseLong (nth toks 1))
              jres (Long/parseLong (nth toks 2))
              kt (Long/parseLong (nth toks 3))
              k (Long/parseLong (nth toks 4))
              seglen (max 0 (* 2 m))
              sp (make-slepian m jres kt)
              ok (atom (some? sp))
              pos (atom 5)]
          (when @ok
            (dotimes [_ k]
              (when @ok
                (let [seg (double-array (map #(Double/parseDouble %)
                                             (subvec toks @pos (+ @pos seglen))))]
                  (swap! pos + seglen)
                  (when-not (try (slepian-addseg sp seg)
                                 (catch clojure.lang.ExceptionInfo _ false))
                    (reset! ok false))))))
          (if @ok
            (let [spec (spect-spectrum (:st sp))]
              (if spec (emit-spec spec) (emit-zeros m)))
            (emit-zeros m)))
        (= op "LONGDATA")
        (let [m (Long/parseLong (nth toks 1))
              w (nth toks 2)
              nt (Long/parseLong (nth toks 3))
              nn (max 0 nt)
              data (double-array (map #(Double/parseDouble %)
                                      (subvec toks 4 (+ 4 nn))))
              wt (get {"SQUARE" 0 "BARTLETT" 1 "WELCH" 2 "HANN" 3} w -1)
              st (when (>= wt 0) (make-spect m wt))]
          (if (nil? st)
            (emit-zeros m)
            (let [nk (quot (dec nt) m)
                  dl (if (> nk 1) (/ (double (- nt (* 2 m))) (double (dec nk))) 0.0)
                  ok (atom true)]
              (if (< nt (* 2 m))
                (emit-zeros m)
                (do
                  (dotimes [kk nk]
                    (when @ok
                      (let [noff (long (+ (* (double kk) dl) 0.5))
                            seg (double-array (map #(aget ^doubles data (+ noff %))
                                                   (range (* 2 m))))]
                        (when-not (try (spect-addseg st seg)
                                       (catch clojure.lang.ExceptionInfo _ false))
                          (reset! ok false)))))
                  (if @ok
                    (let [spec (spect-spectrum st)]
                      (if spec (emit-spec spec) (emit-zeros m)))
                    (emit-zeros m)))))))
        (or (= op "PGRAM") (= op "OVERLAP"))
        (let [m (Long/parseLong (nth toks 1))
              w (nth toks 2)
              k (Long/parseLong (nth toks 3))
              seglen (max 0 (if (= op "PGRAM") (* 2 m) m))
              wt (get {"SQUARE" 0 "BARTLETT" 1 "WELCH" 2 "HANN" 3} w -1)
              st (when (>= wt 0) (make-spect m wt))
              fullseg (double-array (* 2 (max 0 m)))
              first (atom true)
              ok (atom (some? st))
              pos (atom 4)]
          (when @ok
            (dotimes [_ k]
              (when @ok
                (let [seg (double-array (map #(Double/parseDouble %)
                                             (subvec toks @pos (+ @pos seglen))))]
                  (swap! pos + seglen)
                  (if (= op "PGRAM")
                    (when-not (try (spect-addseg st seg)
                                   (catch clojure.lang.ExceptionInfo _ false))
                      (reset! ok false))
                    ;; overlap: shift-and-pair inline
                    (if (not= (alength ^doubles seg) m)
                      (reset! ok false)
                      (if @first
                        (do (dotimes [i m]
                              (aset ^doubles fullseg (+ i m)
                                    (aget ^doubles seg i)))
                            (reset! first false))
                        (do (dotimes [i m]
                              (aset ^doubles fullseg i
                                    (aget ^doubles fullseg (+ i m)))
                              (aset ^doubles fullseg (+ i m)
                                    (aget ^doubles seg i)))
                            (when-not (try (spect-addseg st fullseg)
                                           (catch clojure.lang.ExceptionInfo _ false))
                              (reset! ok false))))))))))
          (if @ok
            (let [spec (spect-spectrum st)]
              (if spec (emit-spec spec) (emit-zeros m)))
            (emit-zeros m)))))))
