;; SHA-256 from scratch (FIPS 180-4): Merkle-Damgard over the 64-round
;; compression function. All words masked to 32 bits (long arithmetic).

(def K [0x428A2F98 0x71374491 0xB5C0FBCF 0xE9B5DBA5 0x3956C25B 0x59F111F1 0x923F82A4 0xAB1C5ED5
        0xD807AA98 0x12835B01 0x243185BE 0x550C7DC3 0x72BE5D74 0x80DEB1FE 0x9BDC06A7 0xC19BF174
        0xE49B69C1 0xEFBE4786 0x0FC19DC6 0x240CA1CC 0x2DE92C6F 0x4A7484AA 0x5CB0A9DC 0x76F988DA
        0x983E5152 0xA831C66D 0xB00327C8 0xBF597FC7 0xC6E00BF3 0xD5A79147 0x06CA6351 0x14292967
        0x27B70A85 0x2E1B2138 0x4D2C6DFC 0x53380D13 0x650A7354 0x766A0ABB 0x81C2C92E 0x92722C85
        0xA2BFE8A1 0xA81A664B 0xC24B8B70 0xC76C51A3 0xD192E819 0xD6990624 0xF40E3585 0x106AA070
        0x19A4C116 0x1E376C08 0x2748774C 0x34B0BCB5 0x391C0CB3 0x4ED8AA4A 0x5B9CCA4F 0x682E6FF3
        0x748F82EE 0x78A5636F 0x84C87814 0x8CC70208 0x90BEFFFA 0xA4506CEB 0xBEF9A3F7 0xC67178F2])

(defn rotr [x n]
  (bit-and (bit-or (unsigned-bit-shift-right x n) (bit-shift-left x (- 32 n))) 0xFFFFFFFF))

(defn compress [h block]
  (let [w0 (mapv (fn [i]
                   (+ (bit-shift-left (nth block (* i 4)) 24)
                      (bit-shift-left (nth block (+ (* i 4) 1)) 16)
                      (bit-shift-left (nth block (+ (* i 4) 2)) 8)
                      (nth block (+ (* i 4) 3))))
                 (range 16))
        w (loop [i 16
                 w w0]
            (if (>= i 64)
              w
              (let [x15 (nth w (- i 15))
                    x2 (nth w (- i 2))
                    s0 (bit-xor (bit-xor (rotr x15 7) (rotr x15 18)) (unsigned-bit-shift-right x15 3))
                    s1 (bit-xor (bit-xor (rotr x2 17) (rotr x2 19)) (unsigned-bit-shift-right x2 10))]
                (recur (inc i)
                       (conj w (bit-and (+ (nth w (- i 16)) s0 (nth w (- i 7)) s1) 0xFFFFFFFF))))))]
    (loop [i 0
           a (nth h 0) b (nth h 1) c (nth h 2) d (nth h 3)
           e (nth h 4) f (nth h 5) g (nth h 6) hh (nth h 7)]
      (if (>= i 64)
        [(bit-and (+ (nth h 0) a) 0xFFFFFFFF) (bit-and (+ (nth h 1) b) 0xFFFFFFFF)
         (bit-and (+ (nth h 2) c) 0xFFFFFFFF) (bit-and (+ (nth h 3) d) 0xFFFFFFFF)
         (bit-and (+ (nth h 4) e) 0xFFFFFFFF) (bit-and (+ (nth h 5) f) 0xFFFFFFFF)
         (bit-and (+ (nth h 6) g) 0xFFFFFFFF) (bit-and (+ (nth h 7) hh) 0xFFFFFFFF)]
        (let [s1 (bit-xor (bit-xor (rotr e 6) (rotr e 11)) (rotr e 25))
              ch (bit-xor (bit-and e f) (bit-and (bit-xor e 0xFFFFFFFF) g))
              t1 (bit-and (+ hh s1 ch (nth K i) (nth w i)) 0xFFFFFFFF)
              s0 (bit-xor (bit-xor (rotr a 2) (rotr a 13)) (rotr a 22))
              maj (bit-xor (bit-xor (bit-and a b) (bit-and a c)) (bit-and b c))
              t2 (bit-and (+ s0 maj) 0xFFFFFFFF)]
          (recur (inc i) (bit-and (+ t1 t2) 0xFFFFFFFF) a b c
                 (bit-and (+ d t1) 0xFFFFFFFF) e f g))))))

(defn sha256-bytes [msg]
  (let [ml (* (count msg) 8)
        padded (loop [p (conj (vec msg) 0x80)]
                 (if (= (rem (count p) 64) 56)
                   p
                   (recur (conj p 0x00))))
        padded (vec (concat padded (map #(bit-and (bit-shift-right ml %) 0xFF) [56 48 40 32 24 16 8 0])))
        h0 [0x6A09E667 0xBB67AE85 0x3C6EF372 0xA54FF53A 0x510E527F 0x9B05688C 0x1F83D9AB 0x5BE0CD19]]
    (loop [off 0
           h h0]
      (if (>= off (count padded))
        (vec (mapcat (fn [v] [(bit-and (unsigned-bit-shift-right v 24) 0xFF)
                              (bit-and (unsigned-bit-shift-right v 16) 0xFF)
                              (bit-and (unsigned-bit-shift-right v 8) 0xFF)
                              (bit-and v 0xFF)])
                     h))
        (recur (+ off 64) (compress h (subvec padded off (+ off 64))))))))


;; Overflow-free modular arithmetic on longs (all RSA values fit in
;; i64; clojure-rs has no bigint-from-string, so no BigInt is used).
(defn addmod [a b m]
  (let [d (- m b)]
    (if (<= d a) (- a d) (+ a b))))

(defn mulmod [a b m]
  (loop [a (mod a m) b (mod b m) r 0]
    (if (zero? b) r
        (recur (addmod a a m) (quot b 2) (if (odd? b) (addmod r a m) r)))))

(defn modpow [b e m]
  (loop [r (mod 1 m) b (mod b m) e e]
    (if (zero? e) r
        (recur (if (odd? e) (mulmod r b m) r)
               (mulmod b b m) (quot e 2)))))

(defn bitlen [n]
  (loop [x n c 0]
    (if (zero? x) c (recur (quot x 2) (inc c)))))

(defn parse-hex [s]
  (mapv (fn [i] (+ (* (Character/digit ^char (nth s i) 16) 16)
                   (Character/digit ^char (nth s (inc i)) 16)))
        (range 0 (count s) 2)))

(defn hash-mod [msg n]
  (reduce (fn [h b] (addmod (mulmod h 256 n) b n)) 0 (sha256-bytes msg)))


(def HLEN 2)

(defn mgf1 [seed outlen]
  (loop [t [] c 0]
    (if (>= (count t) outlen) (subvec (vec t) 0 outlen)
        (recur (into t (sha256-bytes (vec (concat seed [(bit-and (bit-shift-right c 24) 0xFF)
                                                         (bit-and (bit-shift-right c 16) 0xFF)
                                                         (bit-and (bit-shift-right c 8) 0xFF)
                                                         (bit-and c 0xFF)]))))
               (inc c)))))

(defn oaep-encode [msg seed k]
  (let [lhash (subvec (sha256-bytes []) 0 HLEN)
        db (vec (concat lhash (repeat (- k (count msg) (* 2 HLEN) 2) 0) [1] msg))
        masked-db (mapv bit-xor db (mgf1 seed (- k HLEN 1)))
        masked-seed (mapv bit-xor seed (mgf1 masked-db HLEN))]
    (vec (concat [0] masked-seed masked-db))))

(defn oaep-decode [em k]
  (when (and (= (count em) k) (zero? (first em)))
    (let [masked-seed (subvec em 1 (+ 1 HLEN))
          masked-db (subvec em (+ 1 HLEN))
          seed (mapv bit-xor masked-seed (mgf1 masked-db HLEN))
          db (mapv bit-xor masked-db (mgf1 seed (- k HLEN 1)))]
      (when (= (subvec db 0 HLEN) (subvec (sha256-bytes []) 0 HLEN))
        (let [rest (subvec db HLEN)
              i (count (take-while zero? rest))]
          (when (and (< i (count rest)) (= (nth rest i) 1))
            (subvec rest (inc i))))))))

(defn os2ip [em]
  (reduce (fn [v b] (+ (* v 256) b)) 0 em))

(defn i2osp [v k]
  (mapv (fn [i] (mod (quot v (reduce * (repeat (- (dec k) i) 256))) 256)) (range k)))

(defn hex-of [bs]
  (apply str (map #(format "%02x" %) bs)))

(let [toks (vec (.split (.trim (slurp *in*)) " " -1)) op (first toks)]
  (when (>= (count toks) 4)
    (let [n (Long/parseLong (second toks))
          k (quot (+ (bitlen n) 7) 8)]
      (if (= op "ENC")
        (let [e (Long/parseLong (nth toks 2))
              msg (parse-hex (nth toks 3 ""))
              seed (parse-hex (nth toks 4 ""))]
          (print (modpow (os2ip (oaep-encode msg seed k)) e n)))
        (let [d (Long/parseLong (nth toks 2))
              em (i2osp (modpow (Long/parseLong (nth toks 3)) d n) k)
              msg (oaep-decode em k)]
          (print (if (nil? msg) "ERROR" (hex-of msg))))))))
