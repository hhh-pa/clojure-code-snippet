;; Poly1305 one-time authenticator (RFC 8439): Horner evaluation mod
;; 2^130-5 on five 26-bit limbs (long arithmetic), freeze, add s.
(require '[clojure.string :as str])

(def mask26 (dec (bit-shift-left 1 26)))

(defn load-r-limbs [key]
  (let [t0 (+ (nth key 0) (bit-shift-left (nth key 1) 8) (bit-shift-left (nth key 2) 16) (bit-shift-left (nth key 3) 24))
        t1 (+ (unsigned-bit-shift-right (nth key 3) 2) (bit-shift-left (nth key 4) 6)
              (bit-shift-left (nth key 5) 14) (bit-shift-left (nth key 6) 22))
        t2 (+ (unsigned-bit-shift-right (nth key 6) 4) (bit-shift-left (nth key 7) 4)
              (bit-shift-left (nth key 8) 12) (bit-shift-left (nth key 9) 20))
        t3 (+ (unsigned-bit-shift-right (nth key 9) 6) (bit-shift-left (nth key 10) 2)
              (bit-shift-left (nth key 11) 10) (bit-shift-left (nth key 12) 18))
        t4 (+ (nth key 13) (bit-shift-left (nth key 14) 8) (bit-shift-left (nth key 15) 16))]
    [(bit-and t0 mask26) (bit-and t1 mask26 0x3FFFF03) (bit-and t2 mask26 0x3FFC0FF)
     (bit-and t3 mask26 0x3F03FFF) (bit-and t4 mask26 0x00FFFFF)]))

(defn load-block-limbs [msg off blen]
  (let [lo (loop [i 0
                  v 0]
             (if (or (>= i blen) (>= i 8))
               v
               (recur (inc i) (bit-or v (bit-shift-left (nth msg (+ off i)) (* i 8))))))
        hi (loop [i 8
                  v 0]
             (if (>= i blen)
               v
               (recur (inc i) (bit-or v (bit-shift-left (nth msg (+ off i)) (* (- i 8) 8))))))
        b [(bit-and lo mask26) (bit-and (unsigned-bit-shift-right lo 26) mask26)
           (bit-and (bit-or (unsigned-bit-shift-right lo 52) (bit-shift-left hi 12)) mask26)
           (bit-and (unsigned-bit-shift-right hi 14) mask26)
           (bit-and (unsigned-bit-shift-right hi 40) mask26)]
        top (* blen 8)]
    (if (< top 128)
      (assoc b (quot top 26) (bit-or (nth b (quot top 26)) (bit-shift-left 1 (rem top 26))))
      (assoc b 4 (bit-or (nth b 4) (bit-shift-left 1 24))))))

(defn freeze-limbs [h]
  (let [p [0x3FFFFFB mask26 mask26 mask26 mask26]
        [g borrow] (loop [i 0
                          g []
                          borrow 0]
                     (if (>= i 5)
                       [g borrow]
                       (if (< (nth h i) (+ (nth p i) borrow))
                         (recur (inc i) (conj g (+ (nth h i) (bit-shift-left 1 26) (- (nth p i)) (- borrow))) 1)
                         (recur (inc i) (conj g (- (nth h i) (nth p i) borrow)) 0))))]
    (if (zero? borrow) g h)))

(defn poly-tag [key msg]
  (let [[r0 r1 r2 r3 r4] (load-r-limbs key)
        s1 (* r1 5)
        s2 (* r2 5)
        s3 (* r3 5)
        s4 (* r4 5)
        h (loop [off 0
                 h [0 0 0 0 0]]
            (if (>= off (count msg))
              h
              (let [blen (min 16 (- (count msg) off))
                    b (load-block-limbs msg off blen)
                    h (mapv + h b)
                    d [(+ (* (nth h 0) r0) (* (nth h 1) s4) (* (nth h 2) s3) (* (nth h 3) s2) (* (nth h 4) s1))
                       (+ (* (nth h 0) r1) (* (nth h 1) r0) (* (nth h 2) s4) (* (nth h 3) s3) (* (nth h 4) s2))
                       (+ (* (nth h 0) r2) (* (nth h 1) r1) (* (nth h 2) r0) (* (nth h 3) s4) (* (nth h 4) s3))
                       (+ (* (nth h 0) r3) (* (nth h 1) r2) (* (nth h 2) r1) (* (nth h 3) r0) (* (nth h 4) s4))
                       (+ (* (nth h 0) r4) (* (nth h 1) r3) (* (nth h 2) r2) (* (nth h 3) r1) (* (nth h 4) r0))]
                    h (loop [i 0
                             h h
                             dd d]
                        (if (>= i 4)
                          (let [c4 (unsigned-bit-shift-right (nth dd 4) 26)
                                h (assoc h 4 (bit-and (nth dd 4) mask26))
                                h (assoc h 0 (+ (nth h 0) (* c4 5)))
                                c0 (unsigned-bit-shift-right (nth h 0) 26)]
                            (assoc h 0 (bit-and (nth h 0) mask26) 1 (+ (nth h 1) c0)))
                          (let [c (unsigned-bit-shift-right (nth dd i) 26)]
                            (recur (inc i) (assoc h i (bit-and (nth dd i) mask26)) (assoc dd (inc i) (+ (nth dd (inc i)) c))))))]
                (recur (+ off 16) h))))
        h (freeze-limbs h)
        out (loop [limbs h
                   acc 0
                   bits 0
                   out []]
              (if (empty? limbs)
                out
                (let [[acc bits out]
                      (loop [acc (bit-or acc (bit-shift-left (first limbs) bits))
                             bits (+ bits 26)
                             out out]
                        (if (< bits 8)
                          [acc bits out]
                          (recur (unsigned-bit-shift-right acc 8)
                                 (- bits 8)
                                 (conj out (bit-and acc 0xFF)))))]
                  (recur (rest limbs) acc bits out))))
        out (loop [i 0
                   carry 0
                   out out]
              (if (>= i 16)
                out
                (let [v (+ (nth out i) (nth key (+ 16 i)) carry)]
                  (recur (inc i) (unsigned-bit-shift-right v 8) (assoc out i (bit-and v 0xFF))))))]
    (apply str (map #(format "%02x" %) (take 16 out)))))

(defn hex-val [c]
  (Character/digit ^char c 16))

(defn parse-hex [s]
  (mapv (fn [i] (+ (* (hex-val (nth s i)) 16) (hex-val (nth s (inc i))))) (range 0 (count s) 2)))

(defn -main
  "Read key and message hex; print the hex tag."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 1)
      (let [key (parse-hex (nth toks 0))
            msg (if (> (count toks) 1) (parse-hex (nth toks 1)) [])]
        (print (poly-tag key msg))))))

(-main (slurp *in*))
