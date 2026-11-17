;; AES-128 ECB/CBC/CTR modes (SP 800-38A), from-scratch AES encrypt/decrypt.

(def SBOX [99, 124, 119, 123, 242, 107, 111, 197, 48, 1, 103, 43, 254, 215, 171, 118, 202, 130, 201, 125, 250, 89, 71, 240, 173, 212, 162, 175, 156, 164, 114, 192, 183, 253, 147, 38, 54, 63, 247, 204, 52, 165, 229, 241, 113, 216, 49, 21, 4, 199, 35, 195, 24, 150, 5, 154, 7, 18, 128, 226, 235, 39, 178, 117, 9, 131, 44, 26, 27, 110, 90, 160, 82, 59, 214, 179, 41, 227, 47, 132, 83, 209, 0, 237, 32, 252, 177, 91, 106, 203, 190, 57, 74, 76, 88, 207, 208, 239, 170, 251, 67, 77, 51, 133, 69, 249, 2, 127, 80, 60, 159, 168, 81, 163, 64, 143, 146, 157, 56, 245, 188, 182, 218, 33, 16, 255, 243, 210, 205, 12, 19, 236, 95, 151, 68, 23, 196, 167, 126, 61, 100, 93, 25, 115, 96, 129, 79, 220, 34, 42, 144, 136, 70, 238, 184, 20, 222, 94, 11, 219, 224, 50, 58, 10, 73, 6, 36, 92, 194, 211, 172, 98, 145, 149, 228, 121, 231, 200, 55, 109, 141, 213, 78, 169, 108, 86, 244, 234, 101, 122, 174, 8, 186, 120, 37, 46, 28, 166, 180, 198, 232, 221, 116, 31, 75, 189, 139, 138, 112, 62, 181, 102, 72, 3, 246, 14, 97, 53, 87, 185, 134, 193, 29, 158, 225, 248, 152, 17, 105, 217, 142, 148, 155, 30, 135, 233, 206, 85, 40, 223, 140, 161, 137, 13, 191, 230, 66, 104, 65, 153, 45, 15, 176, 84, 187, 22])
(def RCON [1, 2, 4, 8, 16, 32, 64, 128, 27, 54])
(defn hex-val [c] (Character/digit ^char c 16))

(defn parse-hex [s]
  (if (empty? s) [] (mapv (fn [i] (+ (* (hex-val (nth s i)) 16) (hex-val (nth s (inc i))))) (range 0 (count s) 2))))

(defn hex-of [bs] (apply str (map #(format "%02x" %) bs)))

(defn xtime [a] (if (zero? (bit-and a 0x80)) (bit-and (bit-shift-left a 1) 0xff) (bit-and (bit-xor (bit-shift-left a 1) 0x1b) 0xff)))

(defn expand-key [key]
  (loop [rk (vec (concat key (repeat 160 0))) i 16]
    (if (>= i 176) rk
        (if (zero? (mod i 16))
          (let [k (- (quot i 16) 1) b0 (nth rk (- i 4)) b1 (nth rk (- i 3)) b2 (nth rk (- i 2)) b3 (nth rk (- i 1))]
            (recur (-> rk (assoc i (bit-xor (nth rk (- i 16)) (nth SBOX b1) (nth RCON k)))
                       (assoc (+ i 1) (bit-xor (nth rk (- i 15)) (nth SBOX b2)))
                       (assoc (+ i 2) (bit-xor (nth rk (- i 14)) (nth SBOX b3)))
                       (assoc (+ i 3) (bit-xor (nth rk (- i 13)) (nth SBOX b0)))) (+ i 4)))
          (recur (assoc rk i (bit-xor (nth rk (- i 16)) (nth rk (- i 4)))) (inc i))))))

(defn shift-rows [st]
  (vec (for [c (range 4) r (range 4)] (nth st (+ r (* 4 (mod (+ c r) 4)))))))

(defn mix-cols [st]
  (vec (mapcat (fn [c] (let [a0 (nth st (* 4 c)) a1 (nth st (+ (* 4 c) 1)) a2 (nth st (+ (* 4 c) 2)) a3 (nth st (+ (* 4 c) 3))
                            t (bit-xor a0 a1 a2 a3)]
                        [(bit-xor a0 t (xtime (bit-xor a0 a1))) (bit-xor a1 t (xtime (bit-xor a1 a2)))
                         (bit-xor a2 t (xtime (bit-xor a2 a3))) (bit-xor a3 t (xtime (bit-xor a3 a0)))]))
               (range 4))))

(defn block-enc [rk blk]
  (let [st0 (mapv bit-xor blk (subvec rk 0 16))
        st (reduce (fn [s rnd] (mapv bit-xor (mix-cols (mapv SBOX (shift-rows s))) (subvec rk (* rnd 16) (+ (* rnd 16) 16)))) st0 (range 1 10))]
    (mapv bit-xor (mapv SBOX (shift-rows st)) (subvec rk 160 176))))

(defn aes-enc [key blk] (block-enc (expand-key key) blk))

(def BI0 BigInteger/ZERO)
(def RPOLY (.shiftLeft (BigInteger. "e1" 16) 120))

(defn bytes-to-int [bs]
  (reduce (fn [r b] (.add (.shiftLeft r 8) (BigInteger/valueOf (long b)))) BI0 bs))

(defn ghash-mul [x y]
  (loop [i 0 z BI0 v y]
    (if (>= i 128) (.and z (.subtract (.shiftLeft BigInteger/ONE 128) BigInteger/ONE))
        (let [z2 (if (.testBit x (- 127 i)) (.xor z v) z)
              lsb (.testBit v 0) v2 (.shiftRight v 1)]
          (recur (inc i) z2 (if lsb (.xor v2 RPOLY) v2))))))

(defn pad16 [bs] (into (vec bs) (repeat (mod (- (count bs)) 16) 0)))

(defn ghash [h aad ct]
  (let [data (vec (concat (pad16 aad) (pad16 ct)))
        y (reduce (fn [acc blk] (ghash-mul (.xor acc (bytes-to-int blk)) h)) BI0 (partition 16 data))
        lens (.add (.shiftLeft (BigInteger/valueOf (* 8 (count aad))) 64)
                   (BigInteger/valueOf (* 8 (count ct))))]
    (ghash-mul (.xor y lens) h)))

(defn int-to-bytes [x]
  (mapv (fn [i] (.intValue (.and (.shiftRight x (* 8 (- 15 i))) (BigInteger/valueOf 255)))) (range 16)))

(defn gctr [rk j0 data]
  (loop [i 0 ctr (vec j0) out []]
    (if (>= i (count data)) out
        (let [ctr2 (loop [v ctr j 15] (if (< j 0) v (let [b (mod (inc (nth v j)) 256)]
                                                     (if (zero? b) (recur (assoc v j b) (dec j)) (assoc v j b)))))
              ks (block-enc rk ctr2) chunk (subvec data i (min (+ i 16) (count data)))]
          (recur (+ i 16) ctr2 (into out (mapv bit-xor chunk (subvec ks 0 (count chunk)))))))))

(let [toks (vec (.split (.trim (slurp *in*)) " " -1)) op (first toks)
      rk (expand-key (parse-hex (second toks)))
      j0 (vec (concat (parse-hex (nth toks 2)) [0 0 0 1]))
      h (bytes-to-int (block-enc rk (vec (repeat 16 0))))
      s0 (block-enc rk j0)]
  (if (= op "GCM_ENC")
    (let [pt (parse-hex (nth toks 3 "")) aad (parse-hex (nth toks 4 "")) ct (gctr rk j0 pt)]
      (print (str (hex-of ct) " " (hex-of (int-to-bytes (.xor (ghash h aad ct) (bytes-to-int s0)))))))
    (let [ct (parse-hex (nth toks 3 "")) tag (parse-hex (nth toks 4 "")) aad (parse-hex (nth toks 5 ""))
          expect (int-to-bytes (.xor (ghash h aad ct) (bytes-to-int s0)))]
      (if (= expect tag) (print (hex-of (gctr rk j0 ct))) (print "FAIL")))))
