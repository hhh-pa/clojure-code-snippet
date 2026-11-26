;; Radix-2 complex FFT (forward and inverse) by the Danielson-Lanczos
;; lemma with bit-reversal reorder and trigonometric recurrence.
;; Reference: Press et al., Numerical Recipes 3e, Section 12.2 (four1).
;; Documented decisions: the book's truncated literal 6.28318530717959
;; is used verbatim (not full-precision 2*pi); the inverse transform is
;; NOT normalized (returns N times the inverse, as the book does --
;; "you do that yourself"); n < 2 or not a power of 2 throws in the
;; book, reported here as zero values with flag 1.
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

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)
          n (Long/parseLong (nth toks 1))]
      (when (or (= op "FFT") (= op "IFFT"))
        (let [isign (if (= op "FFT") 1 -1)
              len (max 0 (* 2 n))
              data (double-array (map #(Double/parseDouble %)
                                      (subvec toks 2 (+ 2 len))))]
          (if (four1 data n isign)
            (println (str (str/join " " (map #(fmt6 (canon (aget ^doubles data %)))
                                              (range len)))
                          (if (> len 0) " 0" "0")))
            (println (str (str/join " " (concat (repeat len "0.000000") ["1"]))))))))))
