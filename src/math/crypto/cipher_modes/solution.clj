;; AES-128 ECB/CBC/CTR modes (SP 800-38A), from-scratch AES encrypt/decrypt.

(def SBOX [99, 124, 119, 123, 242, 107, 111, 197, 48, 1, 103, 43, 254, 215, 171, 118, 202, 130, 201, 125, 250, 89, 71, 240, 173, 212, 162, 175, 156, 164, 114, 192, 183, 253, 147, 38, 54, 63, 247, 204, 52, 165, 229, 241, 113, 216, 49, 21, 4, 199, 35, 195, 24, 150, 5, 154, 7, 18, 128, 226, 235, 39, 178, 117, 9, 131, 44, 26, 27, 110, 90, 160, 82, 59, 214, 179, 41, 227, 47, 132, 83, 209, 0, 237, 32, 252, 177, 91, 106, 203, 190, 57, 74, 76, 88, 207, 208, 239, 170, 251, 67, 77, 51, 133, 69, 249, 2, 127, 80, 60, 159, 168, 81, 163, 64, 143, 146, 157, 56, 245, 188, 182, 218, 33, 16, 255, 243, 210, 205, 12, 19, 236, 95, 151, 68, 23, 196, 167, 126, 61, 100, 93, 25, 115, 96, 129, 79, 220, 34, 42, 144, 136, 70, 238, 184, 20, 222, 94, 11, 219, 224, 50, 58, 10, 73, 6, 36, 92, 194, 211, 172, 98, 145, 149, 228, 121, 231, 200, 55, 109, 141, 213, 78, 169, 108, 86, 244, 234, 101, 122, 174, 8, 186, 120, 37, 46, 28, 166, 180, 198, 232, 221, 116, 31, 75, 189, 139, 138, 112, 62, 181, 102, 72, 3, 246, 14, 97, 53, 87, 185, 134, 193, 29, 158, 225, 248, 152, 17, 105, 217, 142, 148, 155, 30, 135, 233, 206, 85, 40, 223, 140, 161, 137, 13, 191, 230, 66, 104, 65, 153, 45, 15, 176, 84, 187, 22])
(def RCON [1, 2, 4, 8, 16, 32, 64, 128, 27, 54])
(def ISBOX (let [inv (int-array 256)] (doseq [i (range 256)] (aset inv (nth SBOX i) i)) (vec inv)))

(defn hex-val [c] (Character/digit ^char c 16))

(defn parse-hex [s]
  (if (empty? s) [] (mapv (fn [i] (+ (* (hex-val (nth s i)) 16) (hex-val (nth s (inc i))))) (range 0 (count s) 2))))

(defn hex-of [bs] (apply str (map #(format "%02x" %) bs)))

(defn xtime [a] (if (zero? (bit-and a 0x80)) (bit-and (bit-shift-left a 1) 0xff) (bit-and (bit-xor (bit-shift-left a 1) 0x1b) 0xff)))

(defn gmul [a b]
  (loop [a a b b r 0]
    (if (zero? b) r (recur (xtime a) (unsigned-bit-shift-right b 1) (if (zero? (bit-and b 1)) r (bit-xor r a))))))

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

(defn shift-rows [st inv]
  (vec (for [c (range 4) r (range 4)] (nth st (+ r (* 4 (mod (if inv (- c r) (+ c r)) 4)))))))

(defn mix-cols [st dec]
  (vec (mapcat (fn [c] (let [a0 (nth st (* 4 c)) a1 (nth st (+ (* 4 c) 1)) a2 (nth st (+ (* 4 c) 2)) a3 (nth st (+ (* 4 c) 3))]
                                 (if dec [(bit-xor (gmul a0 14) (gmul a1 11) (gmul a2 13) (gmul a3 9))
                                          (bit-xor (gmul a0 9) (gmul a1 14) (gmul a2 11) (gmul a3 13))
                                          (bit-xor (gmul a0 13) (gmul a1 9) (gmul a2 14) (gmul a3 11))
                                          (bit-xor (gmul a0 11) (gmul a1 13) (gmul a2 9) (gmul a3 14))]
                                      (let [t (bit-xor a0 a1 a2 a3)]
                                        [(bit-xor a0 t (xtime (bit-xor a0 a1))) (bit-xor a1 t (xtime (bit-xor a1 a2)))
                                         (bit-xor a2 t (xtime (bit-xor a2 a3))) (bit-xor a3 t (xtime (bit-xor a3 a0)))]))))
                 (range 4))))

(defn block-crypt [rk blk dec]
  (let [box (if dec ISBOX SBOX) base (if dec 160 0)
        st0 (mapv bit-xor blk (subvec rk base (+ base 16)))
        rounds (if dec (range 9 0 -1) (range 1 10))
        st (reduce (fn [s rnd]
                     (let [sh (mapv box (shift-rows s dec))
                           rkey (subvec rk (* rnd 16) (+ (* rnd 16) 16))]
                       (if dec (mix-cols (mapv bit-xor sh rkey) true)
                           (mapv bit-xor (mix-cols sh false) rkey)))) st0 rounds)
        fin (mapv box (shift-rows st dec)) last (if dec 0 160)]
    (mapv bit-xor fin (subvec rk last (+ last 16)))))

(defn incr-ctr [ctr]
  (loop [v (vec ctr) j 15]
    (if (< j 0) v (let [b (mod (inc (nth v j)) 256)] (if (zero? b) (recur (assoc v j b) (dec j)) (assoc v j b))))))

(let [toks (vec (.split (.trim (slurp *in*)) "\\s+")) op (first toks) rk (expand-key (parse-hex (second toks)))
      has-iv (contains? #{"CBC_ENC" "CBC_DEC" "CTR"} op)
      iv (if has-iv (parse-hex (nth toks 2)) [])
      data (parse-hex (nth toks (if has-iv 3 2) ""))]
  (cond
    (or (= op "ECB_ENC") (= op "ECB_DEC"))
    (print (hex-of (vec (mapcat #(block-crypt rk % (= op "ECB_DEC")) (partition 16 data)))))
    (= op "CBC_ENC")
    (print (hex-of (loop [i 0 prev iv out []] (if (>= i (count data)) out
        (let [blk (mapv bit-xor (subvec data i (+ i 16)) prev) c (block-crypt rk blk false)]
          (recur (+ i 16) c (into out c)))))))
    (= op "CBC_DEC")
    (print (hex-of (loop [i 0 prev iv out []] (if (>= i (count data)) out
        (let [blk (subvec data i (+ i 16)) pt (block-crypt rk blk true)]
          (recur (+ i 16) blk (into out (mapv bit-xor pt prev))))))))
    (= op "CTR")
    (print (hex-of (loop [i 0 ctr iv out []] (if (>= i (count data)) out
        (let [ks (block-crypt rk ctr false) chunk (subvec data i (min (+ i 16) (count data)))]
          (recur (+ i 16) (incr-ctr ctr) (into out (mapv bit-xor chunk (subvec ks 0 (count chunk))))))))))))
