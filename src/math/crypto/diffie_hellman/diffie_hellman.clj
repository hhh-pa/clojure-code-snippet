;; diffie-hellman: Diffie-Hellman key exchange and ElGamal encryption.
;; Input: p g a b m r. Output: A B shared, ElGamal c1 c2, decrypted m.
(ns diffie-hellman (:require [clojure.string :as str]))
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
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (>= (count toks) 6)
      (let [vals (mapv parse-token toks)
            p (nth vals 0) g (nth vals 1) a (nth vals 2)
            b (nth vals 3) m (nth vals 4) rr (nth vals 5)
            A (modpow g a p) B (modpow g b p) s (modpow B a p)
            c1 (modpow g rr p) c2 (mod (* m (modpow B rr p)) p)
            x (mod (nth (egcd (modpow c1 b p) p) 1) p)]
        (println A B s)
        (println c1 c2)
        (println (mod (* c2 x) p))))))
(-main)
