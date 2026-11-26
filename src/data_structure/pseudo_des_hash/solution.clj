;; Pseudo-DES hashing (NR 7.2).
(require '[clojure.string :as str])

(def c1 [0xbaa96887 0x1e17d32c 0x03bcdc3c 0x0f33d1b2])
(def c2 [0x4b0f3b58 0xe874f0c3 0x6955c5a6 0x55a7ca46])
(def m32 0xFFFFFFFF)

(defn psdes [lword rword]
  (loop [i 0 lw lword rw rword]
    (if (>= i 2)
      [lw rw]
      (let [iswap rw
            ia (bit-xor iswap (nth c1 i))
            itmpl (bit-and ia 0xffff)
            itmph (unsigned-bit-shift-right ia 16)
            ib (bit-and (+ (* itmpl itmpl) (bit-and (bit-not (* itmph itmph)) m32)) m32)
            ia (bit-or (unsigned-bit-shift-right ib 16)
                       (bit-and (bit-shift-left (bit-and ib 0xffff) 16) m32))
            rw (bit-xor lw (bit-and (+ (bit-xor ia (nth c2 i)) (* itmpl itmph)) m32))]
        (recur (inc i) iswap rw)))))

(defn hashall [arr]
  (let [m (count arr)]
    (if (< m 2)
      arr
      (let [n0 (dec m)
            n0 (bit-or n0 (unsigned-bit-shift-right n0 1))
            n0 (bit-or n0 (unsigned-bit-shift-right n0 2))
            n0 (bit-or n0 (unsigned-bit-shift-right n0 4))
            n0 (bit-or n0 (unsigned-bit-shift-right n0 8))
            n0 (bit-or n0 (unsigned-bit-shift-right n0 16))
            n (inc n0)]
        (loop [nb n nb2 (quot n 2) a (vec arr)]
          (if (<= nb 1)
            (let [nb2 (quot n 2)]
              (if (= m n)
                a
                (loop [j nb2 a a]
                  (if (>= j m)
                    a
                    (let [[x y] (psdes (nth a j) (nth a (- j nb2)))]
                      (recur (inc j) (assoc a j x (- j nb2) y)))))))
            (recur nb2
                   (quot nb2 2)
                   (loop [jb 0 a a]
                     (if (>= jb (- (+ n 1) nb))
                       a
                       (recur (+ jb nb)
                              (loop [j 0 a a]
                                (if (>= j nb2)
                                  a
                                  (if (< (+ (+ jb j) nb2) m)
                                    (let [[x y] (psdes (nth a (+ jb j)) (nth a (+ (+ jb j) nb2)))]
                                      (recur (inc j) (assoc a (+ jb j) x (+ (+ jb j) nb2) y)))
                                    (recur (inc j) a))))))))))))))

(let [toks (vec (remove str/blank? (str/split (str/trim (slurp *in*)) #"\s+")))]
  (when (seq toks)
    (let [op (nth toks 0)]
      (cond
        (= op "PSDES") (let [[l r] (psdes (Long/parseLong (nth toks 1)) (Long/parseLong (nth toks 2)))]
                         (println (str (bit-and l m32) " " (bit-and r m32))))
        (= op "HASHALL") (let [m (Long/parseLong (nth toks 1))
                               arr (vec (map #(Long/parseLong %) (subvec toks 2 (+ 2 m))))]
                           (println (str/join " " (map #(bit-and % m32) (hashall arr)))))))))
