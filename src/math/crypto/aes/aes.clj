;; aes: AES-128 block cipher encryption (SubBytes, ShiftRows,
;; MixColumns, KeyExpansion). Input: key and plaintext as hex.
;; Output: ciphertext as hex.
(ns aes (:require [clojure.string :as str]))
(def sbox (vec (concat
   [ 99 124 119 123 242 107 111 197  48   1 103  43 254 215 171 118]
   [202 130 201 125 250  89  71 240 173 212 162 175 156 164 114 192]
   [183 253 147  38  54  63 247 204  52 165 229 241 113 216  49  21]
   [  4 199  35 195  24 150   5 154   7  18 128 226 235  39 178 117]
   [  9 131  44  26  27 110  90 160  82  59 214 179  41 227  47 132]
   [ 83 209   0 237  32 252 177  91 106 203 190  57  74  76  88 207]
   [208 239 170 251  67  77  51 133  69 249   2 127  80  60 159 168]
   [ 81 163  64 143 146 157  56 245 188 182 218  33  16 255 243 210]
   [205  12  19 236  95 151  68  23 196 167 126  61 100  93  25 115]
   [ 96 129  79 220  34  42 144 136  70 238 184  20 222  94  11 219]
   [224  50  58  10  73   6  36  92 194 211 172  98 145 149 228 121]
   [231 200  55 109 141 213  78 169 108  86 244 234 101 122 174   8]
   [186 120  37  46  28 166 180 198 232 221 116  31  75 189 139 138]
   [112  62 181 102  72   3 246  14  97  53  87 185 134 193  29 158]
   [225 248 152  17 105 217 142 148 155  30 135 233 206  85  40 223]
   [140 161 137  13 191 230  66 104  65 153  45  15 176  84 187  22]
)))
(def rcon [0x01 0x02 0x04 0x08 0x10 0x20 0x40 0x80 0x1b 0x36])
(defn digit-values [s] (mapv #(Character/digit ^char % 16) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] t)
(defn hexv [c] (Character/digit ^char c 16))
(defn hex-bytes [s] (mapv (fn [i] (+ (* 16 (hexv (nth s (* 2 i)))) (hexv (nth s (inc (* 2 i)))))) (range 16)))
(defn xtime [a] (if (zero? (bit-and a 0x80)) (bit-and (bit-shift-left a 1) 0xff) (bit-and (bit-xor (bit-shift-left a 1) 0x1b) 0xff)))
(defn key-expand [key]
  (loop [i 16 rk (vec key)]
    (if (>= i 176) rk
      (if (zero? (mod i 16))
        (let [b0 (nth rk (- i 4)) b1 (nth rk (- i 3)) b2 (nth rk (- i 2)) b3 (nth rk (- i 1))
              k (- (quot i 16) 1)]
          (recur (+ i 4) (-> rk
                             (conj (bit-xor (nth rk (- i 16)) (bit-xor (nth sbox b1) (nth rcon k))))
                             (conj (bit-xor (nth rk (- i 15)) (nth sbox b2)))
                             (conj (bit-xor (nth rk (- i 14)) (nth sbox b3)))
                             (conj (bit-xor (nth rk (- i 13)) (nth sbox b0))))))
        (recur (inc i) (conj rk (bit-xor (nth rk (- i 16)) (nth rk (- i 4)))))))))
(defn shift-rows [st]
  (mapv (fn [i] (let [r (mod i 4) c (quot i 4)] (nth st (+ r (* 4 (mod (+ c r) 4)))))) (range 16)))
(defn mix-cols [st]
  (vec (mapcat (fn [c]
                 (let [a0 (nth st (* 4 c)) a1 (nth st (+ 1 (* 4 c)))
                       a2 (nth st (+ 2 (* 4 c))) a3 (nth st (+ 3 (* 4 c)))
                       t (bit-xor a0 a1 a2 a3) u a0]
                   [(bit-xor a0 t (xtime (bit-xor a0 a1)))
                    (bit-xor a1 t (xtime (bit-xor a1 a2)))
                    (bit-xor a2 t (xtime (bit-xor a2 a3)))
                    (bit-xor a3 t (xtime (bit-xor a3 u)))]))
               (range 4))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 2)
      (let [rk (key-expand (hex-bytes (nth toks 0)))
            pt (hex-bytes (nth toks 1))
            st0 (mapv bit-xor pt (subvec rk 0 16))
            st9 (loop [rnd 1 st st0]
                  (if (>= rnd 10) st
                    (let [s1 (mapv sbox st)
                          s2 (shift-rows s1)
                          s3 (mix-cols s2)
                          rkseg (subvec rk (* rnd 16) (* (inc rnd) 16))]
                      (recur (inc rnd) (mapv bit-xor s3 rkseg)))))
            s1 (mapv sbox st9)
            s2 (shift-rows s1)
            fin (mapv bit-xor s2 (subvec rk 160 176))
            hx "0123456789abcdef"]
        (println (apply str (mapcat (fn [b] [(nth hx (bit-shift-right b 4)) (nth hx (bit-and b 15))]) fin)))))))
(-main)
