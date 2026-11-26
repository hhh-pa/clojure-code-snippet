;; Sobol quasi-random sequence (NR 7.8).
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

(def maxbit 30)
(def maxdim 6)
(def mdeg [1 2 3 3 4 4])
(def iparr [0 1 1 2 1 4])
(def ivinit [1 1 1 1 1 1 3 1 3 3 1 1 5 7 7 3 3 5 15 11 5 15 13 9])
(def m32 0xFFFFFFFF)

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (and (seq toks) (= (nth toks 0) "SOBOL"))
    (let [dim (Long/parseLong (nth toks 1))
          npoints (Long/parseLong (nth toks 2))
          fac (/ 1.0 (bit-shift-left 1 maxbit))
          iu (loop [k 0 iu (vec (for [j (range maxbit)]
                                  (vec (for [kk (range maxdim)]
                                         (if (< j 4) (nth ivinit (+ (* j maxdim) kk)) 0)))))]
               (if (>= k maxdim)
                 iu
                 (let [iu (loop [j 0 iu iu]
                            (if (>= j (nth mdeg k))
                              iu
                              (recur (inc j)
                                     (assoc-in iu [j k]
                                               (bit-and (bit-shift-left (nth (nth iu j) k)
                                                                        (- (- maxbit 1) j))
                                                        m32)))))]
                   (recur (inc k)
                          (loop [j (nth mdeg k) iu iu]
                            (if (>= j maxbit)
                              iu
                              (let [ipp (nth iparr k)
                                    i (nth (nth iu (- j (nth mdeg k))) k)
                                    i (bit-xor i (unsigned-bit-shift-right i (nth mdeg k)))
                                    [i _] (loop [l (- (nth mdeg k) 1) i i ipp ipp]
                                            (if (< l 1)
                                              [i ipp]
                                              (recur (dec l)
                                                     (if (not= (bit-and ipp 1) 0)
                                                       (bit-xor i (nth (nth iu (- j l)) k))
                                                       i)
                                                     (unsigned-bit-shift-right ipp 1))))]
                                (recur (inc j) (assoc-in iu [j k] (bit-and i m32))))))))))]
      (loop [pt 0 inn 0 ix (vec (repeat maxdim 0)) lines []]
        (if (>= pt npoints)
          (println (str/join "\n" lines))
          (let [[j _] (loop [j 0 im inn]
                        (if (or (>= j maxbit) (= (bit-and im 1) 0))
                          [j im]
                          (recur (inc j) (unsigned-bit-shift-right im 1))))
                ix (vec (for [k (range dim)] (bit-xor (nth ix k) (nth (nth iu j) k))))]
            (recur (inc pt) (inc inn) (vec (concat ix (repeat (- maxdim dim) 0)))
                   (conj lines (str/join " " (map #(fmt6 (canon (* (double %) fac))) ix))))))))))
