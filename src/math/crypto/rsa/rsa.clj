;; rsa: RSA keygen, encryption, and CRT decryption.
;; Input: p q e m. Output: n phi d, ciphertext, decrypted message.
(ns rsa (:require [clojure.string :as str]))
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Long/parseLong t))
(defn modpow [b e m]
  (loop [r (mod 1 m) b (mod b m) e e]
    (if (zero? e) r
      (recur (if (odd? e) (mod (* r b) m) r) (mod (* b b) m) (quot e 2)))))
(defn egcd [a b]
  (if (zero? b) [a 1 0]
    (let [[g x1 y1] (egcd b (mod a b))]
      [g y1 (- x1 (* (quot a b) y1))])))
(defn modinv [a m] (mod (nth (egcd a m) 1) m))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 4)
      (let [vals (mapv parse-token toks)
            p (nth vals 0) q (nth vals 1) e (nth vals 2) m (nth vals 3)
            n (* p q) phi (* (dec p) (dec q)) d (modinv e phi) c (modpow m e n)
            dp (mod d (dec p)) dq (mod d (dec q)) qinv (modinv q p)
            m1 (modpow c dp p) m2 (modpow c dq q)
            h (mod (* qinv (mod (- m1 m2) p)) p)]
        (println n phi d)
        (println c)
        (println (+ m2 (* h q)))))))
(-main)
