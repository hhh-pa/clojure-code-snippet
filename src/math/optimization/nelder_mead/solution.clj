;; Multidimensional minimization by the downhill simplex
;; method of Nelder and Mead (Amoeba).
;; Reference: Press et al., Numerical Recipes 3e, Section 10.5.
;; No online-judge problem found -- fixtures are self-generated
;; (scipy oracle), no header link.
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

(defn dget [a i j] (aget ^doubles (nth a i) j))

(defn dset! [a i j v] (aset ^doubles (nth a i) j (double v)))

(defn mkfunc [fname fp]
  (if (= fname "QUAD")
    (fn [^doubles x]
      (let [n (alength ^doubles x)]
        (loop [i 0 s (nth fp n)]
          (if (>= i n) s
            (let [d (- (aget ^doubles x i) (nth fp i))]
              (recur (inc i) (+ s (* d d))))))))
    (fn [^doubles x]
      (let [n (alength ^doubles x)]
        (loop [i 0 s 0.0]
          (if (>= i (dec n)) s
            (let [a (- (aget ^doubles x (inc i))
                       (* (aget ^doubles x i) (aget ^doubles x i)))
                  b (- 1.0 (aget ^doubles x i))]
              (recur (inc i) (+ s (+ (* (* 100.0 a) a) (* b b)))))))))))

(defn get-psum [p ^doubles psum mpts ndim]
  (dotimes [j ndim]
    (aset ^doubles psum j (loop [i 0 s 0.0]
                               (if (>= i mpts) s (recur (inc i) (+ s (dget p i j))))))))

(defn amotry [f p ^doubles y ^doubles psum ihi fac ndim]
  (let [fac1 (/ (- 1.0 fac) (double ndim))
        fac2 (- fac1 fac)
        ptry (double-array ndim)]
    (dotimes [j ndim]
      (aset ^doubles ptry j (- (* (aget ^doubles psum j) fac1)
                               (* (dget p ihi j) fac2))))
    (let [ytry (f ptry)]
      (when (< ytry (aget ^doubles y ihi))
        (aset ^doubles y ihi ytry)
        (dotimes [j ndim]
          (aset ^doubles psum j (+ (aget ^doubles psum j)
                                   (- (aget ^doubles ptry j) (dget p ihi j))))
          (dset! p ihi j (aget ^doubles ptry j))))
      ytry)))

(defn extrema [^doubles y mpts]
  (let [[ihi0 inhi0] (if (> (aget ^doubles y 0) (aget ^doubles y 1)) [0 1] [1 0])]
    (loop [i 0 ilo 0 ihi ihi0 inhi inhi0]
      (if (>= i mpts)
        [ilo ihi inhi]
        (let [nilo (if (<= (aget ^doubles y i) (aget ^doubles y ilo)) i ilo)
              ni (if (> (aget ^doubles y i) (aget ^doubles y ihi))
                   [i ihi]
                   (if (and (> (aget ^doubles y i) (aget ^doubles y inhi)) (not= i ihi))
                     [ihi i]
                     [ihi inhi]))]
          (recur (inc i) nilo (nth ni 0) (nth ni 1)))))))

(defn amoeba [f point del ftol]
  (let [ndim (count point)
        mpts (inc ndim)
        p (vec (map-indexed (fn [i _]
                              (let [r (double-array point)]
                                (when (pos? i)
                                  (aset ^doubles r (dec i)
                                        (+ (aget ^doubles r (dec i)) del)))
                                r))
                            (range mpts)))
        y (double-array mpts)
        psum (double-array ndim)]
    (dotimes [i mpts] (aset ^doubles y i (f (nth p i))))
    (get-psum p psum mpts ndim)
    (loop [nfunc 0]
      (let [[ilo ihi inhi] (extrema y mpts)
            rtol (/ (* 2.0 (absv (- (aget ^doubles y ihi) (aget ^doubles y ilo))))
                    (+ (+ (absv (aget ^doubles y ihi)) (absv (aget ^doubles y ilo))) 1.0e-10))]
        (if (< rtol ftol)
          (let [tmp (aget ^doubles y 0)]
            (aset ^doubles y 0 (aget ^doubles y ilo))
            (aset ^doubles y ilo tmp)
            (let [pmin (double-array ndim)]
              (dotimes [i ndim]
                (let [t (dget p 0 i)]
                  (dset! p 0 i (dget p ilo i))
                  (dset! p ilo i t)
                  (aset ^doubles pmin i (dget p 0 i))))
              [pmin (aget ^doubles y 0) nfunc 0]))
          (if (>= nfunc 5000)
            [nil 0.0 nfunc 1]
            (let [nf (+ nfunc 2)
                  ytry (amotry f p y psum ihi -1.0 ndim)]
              (cond (<= ytry (aget ^doubles y ilo))
                    (do (amotry f p y psum ihi 2.0 ndim)
                        (recur nf))
                    (>= ytry (aget ^doubles y inhi))
                    (let [ysave (aget ^doubles y ihi)
                          ytry2 (amotry f p y psum ihi 0.5 ndim)]
                      (if (>= ytry2 ysave)
                        (do (dotimes [i mpts]
                              (when (not= i ilo)
                                (dotimes [j ndim]
                                  (let [v (* 0.5 (+ (dget p i j) (dget p ilo j)))]
                                    (dset! p i j v)
                                    (aset ^doubles psum j v)))
                                (aset ^doubles y i (f psum))))
                            (get-psum p psum mpts ndim)
                            (recur (+ nf ndim)))
                        (recur nf)))
                    :else (recur (dec nf))))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          fname (nth toks 1)
          n (Long/parseLong (nth toks 2))
          fpp (if (= fname "QUAD")
                [(vec (map #(Double/parseDouble %) (subvec toks 3 (+ 4 n)))) (+ 4 n)]
                [[] 3])
          fp (nth fpp 0)
          p (nth fpp 1)
          point (vec (map #(Double/parseDouble %) (subvec toks p (+ p n))))
          del (Double/parseDouble (nth toks (+ p n)))
          ftol (Double/parseDouble (nth toks (+ p n 1)))
          res (when (= op "AMOEBA")
                (let [f (mkfunc fname fp)
                      [pmin fmin nfunc flag] (amoeba f point del ftol)]
                  (if (not= flag 0)
                    (str (str/join " " (repeat n "0.000000")) " 0.000000 " nfunc " 1")
                    (str (str/join " " (map #(fmt6 (canon (aget ^doubles pmin %))) (range n)))
                         " " (fmt6 (canon fmin)) " " nfunc " 0"))))]
      (when res (println res)))))
