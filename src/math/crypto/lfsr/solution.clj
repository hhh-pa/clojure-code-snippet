;; Linear feedback shift registers (NR 7.5).
(require '[clojure.string :as str])

(def nn 32)
(def factors [3 5 17 257 65537])
(def m32 0xFFFFFFFF)

(defn matmul-into [^ints dst ^ints x ^ints y]
  (let [^ints tmp (int-array (* nn nn))]
    (loop [i 0]
      (when (< i nn)
        (loop [j 0]
          (when (< j nn)
            (aset tmp (unchecked-add (unchecked-multiply i nn) j)
                  (bit-and (loop [k 0 s 0]
                             (if (>= k nn)
                               s
                               (recur (unchecked-inc k)
                                      (unchecked-add s
                                                     (unchecked-multiply
                                                      (aget x (unchecked-add (unchecked-multiply i nn) k))
                                                      (aget y (unchecked-add (unchecked-multiply k nn) j)))))))
                           1))
            (recur (unchecked-inc j))))
        (recur (unchecked-inc i))))
    (loop [k 0]
      (when (< k (* nn nn))
        (aset dst k (aget tmp k))
        (recur (unchecked-inc k))))
    dst))

(defn primpolytest [n]
  (let [t (int-array (* nn nn))
        a (int-array (* nn nn))
        p (int-array (* nn nn))
        tnm1 (dec (bit-shift-left 1 32))]
    (if (> n (unsigned-bit-shift-right tnm1 1))
      0
      (do
        (dotimes [i nn]
          (when (>= i 1) (aset t (+ (* i nn) (dec i)) 1)))
        (loop [m n j 0]
          (when (not= m 0)
            (when (not= (bit-and m 1) 0) (aset t j 1))
            (recur (unsigned-bit-shift-right m 1) (inc j))))
        (aset t (dec nn) 1)
        (loop [rnd 0]
          (if (>= rnd 6)
            1
            (let [pw (if (= rnd 0) tnm1 (quot tnm1 (nth factors (dec rnd))))]
              (dotimes [k (* nn nn)] (aset a k (aget t k)) (aset p k 0))
              (dotimes [k nn] (aset p (+ (* k nn) k) 1))
              (loop [e pw]
                (when (not= (bit-and e 1) 0) (matmul-into p p a))
                (let [e (unsigned-bit-shift-right e 1)]
                  (when (not= e 0)
                    (matmul-into a a a)
                    (recur e))))
              (let [ident (loop [i 0 ok 1]
                            (if (>= i nn)
                              ok
                              (recur (inc i)
                                     (loop [k 0 ok ok]
                                       (if (>= k nn)
                                         ok
                                         (recur (inc k)
                                                (if (= i k)
                                                  (if (= (aget p (+ (* i nn) k)) 1) ok 0)
                                                  (if (= (aget p (+ (* i nn) k)) 0) ok 0))))))))]
                (cond
                  (and (= rnd 0) (not= ident 1)) 0
                  (and (> rnd 0) (= ident 1)) 0
                  :else (recur (inc rnd)))))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (cond
        (= op "STEP") (let [maskp (Long/parseLong (nth toks 1))
                            maskn (Long/parseLong (nth toks 2))
                            a0 (Long/parseLong (nth toks 3))
                            n (Long/parseLong (nth toks 4))]
                        (println
                         (str/join " "
                          (loop [i 0 a a0 acc []]
                            (if (>= i n)
                              acc
                              (let [a (bit-and (if (not= (bit-and a maskn) 0)
                                                 (bit-or (bit-shift-left (bit-xor a maskp) 1) 1)
                                                 (bit-shift-left a 1))
                                               m32)]
                                (recur (inc i) a (conj acc (str a)))))))))
        (= op "ISPRIMITIVE") (println (primpolytest (Long/parseLong (nth toks 1))))))))
