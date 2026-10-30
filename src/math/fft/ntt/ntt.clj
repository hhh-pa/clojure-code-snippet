;; ntt: forward Number-Theoretic Transform mod 998244353 (root 3).
;; Input: n then n coefficients (n a power of two).
;; Output: n transformed values on one line.
(ns ntt (:require [clojure.string :as str]))
(def ^:const MOD 998244353)
(def ^:const G 3)
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Long/parseLong t))
(defn modpow [b e]
  (loop [r 1 b (mod b MOD) e e]
    (if (zero? e) r
      (recur (if (odd? e) (mod (* r b) MOD) r) (mod (* b b) MOD) (quot e 2)))))
(defn ntt! [^longs a]
  (let [n (alength a)]
    (loop [i 1 j 0]
      (when (< i n)
        (let [jj (loop [jj j bit (quot n 2)]
                   (if (zero? (bit-and jj bit)) (bit-xor jj bit)
                     (recur (bit-xor jj bit) (quot bit 2))))
              jj (bit-xor jj 0)]
          (when (< i jj)
            (let [t (aget a i)] (aset a i (aget a jj)) (aset a jj t)))
          (recur (inc i) jj))))
    (loop [ln 2]
      (when (<= ln n)
        (let [wlen (modpow G (quot (dec MOD) ln))]
          (loop [i 0]
            (when (< i n)
              (loop [j 0 w 1]
                (when (< j (quot ln 2))
                  (let [u (aget a (+ i j))
                        v (mod (* (aget a (+ i j (quot ln 2))) w) MOD)]
                    (aset a (+ i j) (mod (+ u v) MOD))
                    (aset a (+ i j (quot ln 2)) (mod (- u v) MOD))
                    (recur (inc j) (mod (* w wlen) MOD)))))
              (recur (+ i ln)))))
        (recur (* ln 2))))
    a))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (let [vals (mapv parse-token toks)
            n (nth vals 0)
            a (long-array (take n (drop 1 vals)))]
        (ntt! a)
        (println (str/join " " (map str (seq a))))))))
(-main)
