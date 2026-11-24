;; Lomb-Scargle periodogram for unevenly sampled data: direct
;; O(N^2) least-squares fit (period) and the FFT-based fast
;; approximation with extirpolation (fasper/spread).
;; Reference: Press et al., Numerical Recipes 3e, Section 13.9
;; (period/fasper/spread, vendoring avevar from 14.2 and
;; realft+four1 from 12.2-12.3).
;; Documented decisions: n >= 2 enforced (n = 1 makes avevar's
;; variance NaN, n = 0 overruns); nout must satisfy
;; 1 <= nout <= 131072 and fasper's nfreq <= 2^20 (robustness
;; caps on ofac/hifac); period keeps the LAST frequency on ties
;; (>=), fasper the FIRST (>), exactly as the book; abscissas
;; must span a positive range (xdif = 0 is unguarded garbage
;; in the book); exact-zero variance flags (the book throws).
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

(defn avevar [^doubles data]
  (let [n (alength ^doubles data)]
    (when (>= n 2)
      (let [ave (/ (loop [j 0 s 0.0]
                     (if (>= j n) s
                       (recur (inc j) (+ s (aget ^doubles data j)))))
                   (double n))
            [vari ep] (loop [j 0 vari 0.0 ep 0.0]
                        (if (>= j n)
                          [vari ep]
                          (let [s (- (aget ^doubles data j) ave)]
                            (recur (inc j)
                                   (+ vari (* s s))
                                   (+ ep s)))))]
        [ave (/ (- vari (/ (* ep ep) (double n))) (double (dec n)))]))))

(def ^:private nfac [0 1 1 2 6 24 120 720 5040 40320 362880])

(defn spread [yv ^doubles yy x m]
  (if (> m 10)
    false
    (let [n (alength ^doubles yy)
          ix (long x)]
      (if (= x (double ix))
        (aset ^doubles yy (dec ix) (+ (aget ^doubles yy (dec ix)) yv))
        (let [t (- x (* 0.5 (double m)))
              ilo (min (max (long t) 0) (- n m))
              ihi (+ ilo m)
              fac (loop [j (inc ilo) f (- (- x (double ilo)) 1.0)]
                    (if (>= j ihi) f
                      (recur (inc j) (* f (- (- x (double j)) 1.0)))))]
          (aset ^doubles yy (dec ihi)
                (+ (aget ^doubles yy (dec ihi))
                   (/ (* yv fac) (* (double (nth nfac m))
                                    (- x (double ihi))))))
          (loop [j (dec ihi) nden (nth nfac m)]
            (when (> j ilo)
              (let [nden (* (quot nden (- j ilo)) (- j ihi))]
                (aset ^doubles yy (dec j)
                      (+ (aget ^doubles yy (dec j))
                         (/ (* yv fac) (* (double nden)
                                          (- x (double j))))))
                (recur (dec j) nden))))))
      true)))

(def ^:private TWOPI 6.283185307179586476)

(defn lomb-prob [pmax nout ofac]
  (let [expy (Math/exp (- pmax))
        effm (/ (* 2.0 (double nout)) ofac)
        prob (* effm expy)]
    (if (> prob 0.01)
      (- 1.0 (Math/pow (- 1.0 expy) effm))
      prob)))

(defn period [^doubles x ^doubles y ofac hifac]
  (let [n (alength ^doubles x)]
    (when (and (>= n 2) (> ofac 0.0) (> hifac 0.0))
      (let [noutd (* (* 0.5 ofac) hifac (double n))]
        (when (and (>= noutd 1.0) (<= noutd 131072.0))
          (let [nout (long noutd)
                av (avevar y)]
            (when (and av (not= (second av) 0.0))
              (let [[ave vari] av
                    xmin (loop [j 1 v (aget ^doubles x 0)]
                           (if (>= j n) v
                             (recur (inc j) (min v (aget ^doubles x j)))))
                    xmax (loop [j 1 v (aget ^doubles x 0)]
                           (if (>= j n) v
                             (recur (inc j) (max v (aget ^doubles x j)))))
                    xdif (- xmax xmin)]
                (when (> xdif 0.0)
                  (let [wr (double-array n)
                        wi (double-array n)
                        wpr (double-array n)
                        wpi (double-array n)
                        px (double-array nout)
                        py (double-array nout)
                        xave (* 0.5 (+ xmin xmax))
                        p0 (/ 1.0 (* xdif ofac))]
                    (dotimes [j n]
                      (let [wpi0 (* TWOPI (* (- (aget ^doubles x j) xave) p0))
                            s (Math/sin (* 0.5 wpi0))]
                        (aset ^doubles wpr j (* (* -2.0 s) s))
                        (aset ^doubles wpi j (Math/sin wpi0))
                        (aset ^doubles wr j (Math/cos wpi0))
                        (aset ^doubles wi j (Math/sin wpi0))))
                    (let [[jmax pymax]
                          (loop [i 0 jmax 0 pymax 0.0]
                            (if (>= i nout)
                              [jmax pymax]
                              (let [pnow (/ (double (inc i)) (* ofac xdif))]
                                (aset ^doubles px i pnow)
                                (let [[sumsh sumc]
                                      (loop [j 0 sumsh 0.0 sumc 0.0]
                                        (if (>= j n)
                                          [sumsh sumc]
                                          (let [c (Math/cos (* (* pnow TWOPI)
                                                                (- (aget ^doubles x j) xave)))
                                                s (Math/sin (* (* pnow TWOPI)
                                                                (- (aget ^doubles x j) xave)))]
                                            (recur (inc j)
                                                   (+ sumsh (* s c))
                                                   (+ sumc (* (- c s) (+ c s)))))))
                                      wtau (* 0.5 (Math/atan2 (* 2.0 sumsh) sumc))
                                      swtau (Math/sin wtau)
                                      cwtau (Math/cos wtau)
                                      [sums sumc sumsy sumcy]
                                      (loop [j 0 sums 0.0 sumc 0.0 sumsy 0.0 sumcy 0.0]
                                        (if (>= j n)
                                          [sums sumc sumsy sumcy]
                                          (let [ss (- (* (aget ^doubles wr j) swtau)
                                                      (* cwtau (aget ^doubles wi j)))
                                                cc (+ (* (aget ^doubles wr j) cwtau)
                                                      (* swtau (aget ^doubles wi j)))
                                                yy (- (aget ^doubles y j) ave)
                                                wtemp (aget ^doubles wr j)]
                                            (aset ^doubles wr j
                                                  (+ (- (* wtemp (aget ^doubles wpr j))
                                                          (* (aget ^doubles wi j)
                                                             (aget ^doubles wpi j)))
                                                     (aget ^doubles wr j)))
                                            (aset ^doubles wi j
                                                  (+ (+ (* (aget ^doubles wi j)
                                                            (aget ^doubles wpr j))
                                                         (* wtemp (aget ^doubles wpi j)))
                                                     (aget ^doubles wi j)))
                                            (recur (inc j)
                                                   (+ sums (* ss ss))
                                                   (+ sumc (* cc cc))
                                                   (+ sumsy (* yy ss))
                                                   (+ sumcy (* yy cc))))))
                                      pv (/ (* 0.5 (+ (/ (* sumcy sumcy) sumc)
                                                      (/ (* sumsy sumsy) sums)))
                                            vari)]
                                  (aset ^doubles py i pv)
                                  (if (>= pv pymax)
                                    (recur (inc i) i pv)
                                    (recur (inc i) jmax pymax))))))]
                      {:px px :py py :jmax jmax
                       :prob (lomb-prob pymax nout ofac)})))))))))))

(defn fasper [^doubles x ^doubles y ofac hifac]
  (let [n (alength ^doubles x)
        MACC 4]
    (when (and (>= n 2) (> ofac 0.0) (> hifac 0.0))
      (let [noutd (* (* 0.5 ofac) hifac (double n))]
        (when (and (>= noutd 1.0) (<= noutd 131072.0))
          (let [nout (long noutd)
                nfreqt (* (* (* ofac hifac) (double n)) (double MACC))
                nfreq (loop [f 64]
                        (cond (>= (double f) nfreqt) f
                              (> (bit-shift-left f 1) 1048576) nil
                              :else (recur (bit-shift-left f 1))))]
            (when nfreq
              (let [nwk (bit-shift-left nfreq 1)
                    wk1 (double-array nwk)
                    wk2 (double-array nwk)
                    av (avevar y)]
                (when (and av (not= (second av) 0.0))
                  (let [[ave vari] av
                        xmin (loop [j 1 v (aget ^doubles x 0)]
                               (if (>= j n) v
                                 (recur (inc j) (min v (aget ^doubles x j)))))
                        xmax (loop [j 1 v (aget ^doubles x 0)]
                               (if (>= j n) v
                                 (recur (inc j) (max v (aget ^doubles x j)))))
                        xdif (- xmax xmin)]
                    (when (> xdif 0.0)
                      (let [fac (/ (double nwk) (* xdif ofac))
                            fndim (double nwk)
                            df (/ 1.0 (* xdif ofac))
                            px (double-array nout)
                            py (double-array nout)]
                        (dotimes [j n]
                          (let [ck (rem (* (- (aget ^doubles x j) xmin) fac) fndim)
                                ckk (* 2.0 ck)
                                ck (+ ck 1.0)
                                ckk (+ (rem ckk fndim) 1.0)]
                            (spread (- (aget ^doubles y j) ave) wk1 ck MACC)
                            (spread 1.0 wk2 ckk MACC)))
                        (when (and (realft wk1 1) (realft wk2 1))
                          (let [[jmax pmax]
                                (loop [j 0 k 2 jmax 0 pmax -1.0]
                                  (if (>= j nout)
                                    [jmax pmax]
                                    (let [hypo (Math/sqrt (+ (* (aget ^doubles wk2 k)
                                                                  (aget ^doubles wk2 k))
                                                               (* (aget ^doubles wk2 (inc k))
                                                                  (aget ^doubles wk2 (inc k)))))
                                          hc2wt (/ (* 0.5 (aget ^doubles wk2 k)) hypo)
                                          hs2wt (/ (* 0.5 (aget ^doubles wk2 (inc k))) hypo)
                                          cwt (Math/sqrt (+ 0.5 hc2wt))
                                          s (Math/sqrt (- 0.5 hc2wt))
                                          swt (if (< hs2wt 0.0) (- s) s)
                                          den (+ (+ (* 0.5 (double n))
                                                      (* hc2wt (aget ^doubles wk2 k)))
                                                   (* hs2wt (aget ^doubles wk2 (inc k))))
                                          t1 (+ (* cwt (aget ^doubles wk1 k))
                                                  (* swt (aget ^doubles wk1 (inc k))))
                                          cterm (/ (* t1 t1) den)
                                          t2 (- (* cwt (aget ^doubles wk1 (inc k)))
                                                  (* swt (aget ^doubles wk1 k)))
                                          sterm (/ (* t2 t2) (- (double n) den))]
                                      (aset ^doubles px j (* (double (inc j)) df))
                                      (aset ^doubles py j (/ (+ cterm sterm)
                                                              (* 2.0 vari)))
                                      (if (> (aget ^doubles py j) pmax)
                                        (recur (inc j) (+ k 2) j (aget ^doubles py j))
                                        (recur (inc j) (+ k 2) jmax pmax)))))]
                            {:px px :py py :jmax jmax
                             :prob (lomb-prob pmax nout ofac)}))))))))))))))

(defn fail-nout [n ofac hifac]
  (if (and (>= n 2) (> ofac 0.0) (> hifac 0.0))
    (let [nd (* (* 0.5 ofac) hifac (double n))]
      (if (>= nd 1.0)
        (if (<= nd 131072.0) (long nd) 131072)
        0))
    0))

(defn emit-lomb [r]
  (println (str (str/join " "
                          (concat (map #(fmt6 (canon (aget ^doubles (:px r) %)))
                                           (range (alength ^doubles (:px r))))
                                  (map #(fmt6 (canon (aget ^doubles (:py r) %)))
                                           (range (alength ^doubles (:py r))))
                                  [(str (:jmax r))
                                   (fmt6 (canon (:prob r)))
                                   "0"])))))
(defn emit-fail [nout]
  (println (str (str/join " " (concat (repeat (* 2 nout) "0.000000")
                                      ["0" "0.000000" "1"])))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (when (or (= op "LOMB") (= op "FASTLOMB"))
        (let [n (Long/parseLong (nth toks 1))
              nn (max 0 n)
              x (double-array (map #(Double/parseDouble %)
                                   (subvec toks 2 (+ 2 nn))))
              y (double-array (map #(Double/parseDouble %)
                                   (subvec toks (+ 2 nn) (+ 2 (* 2 nn)))))
              ofac (Double/parseDouble (nth toks (+ 2 (* 2 nn))))
              hifac (Double/parseDouble (nth toks (+ 3 (* 2 nn))))
              r (if (= op "LOMB")
                  (period x y ofac hifac)
                  (fasper x y ofac hifac))]
          (if r
            (emit-lomb r)
            (emit-fail (fail-nout n ofac hifac))))))))
