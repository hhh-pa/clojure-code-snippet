;; ChaCha20 stream cipher (RFC 8439): 20-round block function,
;; 32-bit words masked (long arithmetic), counter wraps mod 2^32.
(require '[clojure.string :as str])

(def mask32 0xFFFFFFFF)

(defn hex-val [c]
  (Character/digit ^char c 16))

(defn parse-hex [s]
  (mapv (fn [i] (+ (* (hex-val (nth s i)) 16) (hex-val (nth s (inc i)))))
        (range 0 (count s) 2)))

(defn rotl32 [x n]
  (bit-and (bit-or (bit-shift-left x n) (unsigned-bit-shift-right x (- 32 n))) mask32))

(defn quarter-round [w a b c d]
  (let [w (assoc w a (bit-and (+ (nth w a) (nth w b)) mask32))
        w (assoc w d (bit-xor (nth w d) (nth w a)))
        w (assoc w d (rotl32 (nth w d) 16))
        w (assoc w c (bit-and (+ (nth w c) (nth w d)) mask32))
        w (assoc w b (bit-xor (nth w b) (nth w c)))
        w (assoc w b (rotl32 (nth w b) 12))
        w (assoc w a (bit-and (+ (nth w a) (nth w b)) mask32))
        w (assoc w d (bit-xor (nth w d) (nth w a)))
        w (assoc w d (rotl32 (nth w d) 8))
        w (assoc w c (bit-and (+ (nth w c) (nth w d)) mask32))
        w (assoc w b (bit-xor (nth w b) (nth w c)))
        w (assoc w b (rotl32 (nth w b) 7))]
    w))

(defn chacha-block [key counter nonce]
  (let [le32 (fn [bs o] (+ (nth bs o) (bit-shift-left (nth bs (+ o 1)) 8)
                           (bit-shift-left (nth bs (+ o 2)) 16) (bit-shift-left (nth bs (+ o 3)) 24)))
        st (vec (concat [0x61707865 0x3320646E 0x79622D32 0x6B206574]
                        (map #(le32 key (* % 4)) (range 8))
                        [counter]
                        (map #(le32 nonce (* % 4)) (range 3))))
        w (loop [r 0
                 w st]
            (if (>= r 10)
              w
              (recur (inc r)
                     (-> w
                         (quarter-round 0 4 8 12)
                         (quarter-round 1 5 9 13)
                         (quarter-round 2 6 10 14)
                         (quarter-round 3 7 11 15)
                         (quarter-round 0 5 10 15)
                         (quarter-round 1 6 11 12)
                         (quarter-round 2 7 8 13)
                         (quarter-round 3 4 9 14)))))]
    (vec (mapcat (fn [i]
                   (let [v (bit-and (+ (nth w i) (nth st i)) mask32)]
                     [(bit-and v 0xFF) (bit-and (unsigned-bit-shift-right v 8) 0xFF)
                      (bit-and (unsigned-bit-shift-right v 16) 0xFF)
                      (bit-and (unsigned-bit-shift-right v 24) 0xFF)]))
                 (range 16)))))

(defn -main
  "Read key, nonce, counter, plaintext; print ciphertext as hex."
  [input]
  (let [toks (remove str/blank? (str/split input #"\s+"))]
    (when (>= (count toks) 3)
      (let [key (parse-hex (nth toks 0))
            nonce (parse-hex (nth toks 1))
            counter (Long/parseLong (nth toks 2))
            msg (if (> (count toks) 3) (parse-hex (nth toks 3)) [])
            nblocks (quot (+ (count msg) 63) 64)]
        (print (apply str
                      (mapcat (fn [blk]
                                (let [ks (chacha-block key (rem (+ counter blk) 4294967296) nonce)
                                      chunk (subvec msg (* blk 64) (min (+ (* blk 64) 64) (count msg)))]
                                  (map #(format "%02x" %) (map bit-xor chunk ks))))
                              (range nblocks))))))))

(-main (slurp *in*))
