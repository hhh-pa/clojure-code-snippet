;; lcg-mt: Linear Congruential Generator and Mersenne Twister MT19937.
;; Input: "L a c m seed n" or "M seed n". Output: n values on one line.
(ns lcg-mt (:require [clojure.string :as str]))
(def ^:const MASK 0xffffffff)
(defn digit-values [s] (mapv #(Character/digit ^char % 36) s))
(defn separators [s] (str/split s #"\r?\n"))
(defn tokenize [s]
  (let [t (str/trim s)]
    (if (str/blank? t) [] (vec (str/split t #"\s+")))))
(defn parse-token [t] (Long/parseLong t))
(defn mt-init [seed]
  (loop [i 1 mt (assoc (vec (repeat 624 0)) 0 (bit-and seed MASK))]
    (if (>= i 624) mt
      (recur (inc i) (assoc mt i (bit-and (+ (* 1812433253 (bit-xor (nth mt (dec i)) (bit-shift-right (nth mt (dec i)) 30))) i) MASK))))))
(defn mt-twist [mt]
  (loop [i 0 mt mt]
    (if (>= i 624) mt
      (let [y (bit-and (bit-or (bit-and (nth mt i) 0x80000000) (bit-and (nth mt (mod (inc i) 624)) 0x7fffffff)) MASK)
            v (bit-xor (nth mt (mod (+ i 397) 624)) (bit-shift-right y 1))]
        (recur (inc i) (assoc mt i (if (odd? y) (bit-xor v 0x9908b0df) v)))))))
(defn mt-temper [y]
  (let [y1 (bit-xor y (bit-shift-right y 11))
        y2 (bit-xor y1 (bit-and (bit-shift-left y1 7) 0x9d2c5680))
        y3 (bit-xor y2 (bit-and (bit-shift-left y2 15) 0xefc60000))
        y4 (bit-xor y3 (bit-shift-right y3 18))]
    (bit-and y4 MASK)))
(defn mt-gen [seed n]
  (loop [k 0 mt (mt-init seed) idx 624 acc []]
    (if (>= k n) acc
      (let [[mt idx] (if (>= idx 624) [(mt-twist mt) 0] [mt idx])]
        (recur (inc k) mt (inc idx) (conj acc (mt-temper (nth mt idx))))))))
(defn -main []
  (let [toks (tokenize (slurp *in*))]
    (when (seq toks)
      (if (= "L" (nth toks 0))
        (let [a (parse-token (nth toks 1)) c (parse-token (nth toks 2)) m (parse-token (nth toks 3))
              seed (parse-token (nth toks 4)) n (parse-token (nth toks 5))]
          (println (str/join " " (map str (loop [i 0 x seed acc []]
                                            (if (>= i n) acc
                                              (let [nx (mod (+ (* a x) c) m)]
                                                (recur (inc i) nx (conj acc nx)))))))))
        (let [seed (parse-token (nth toks 1)) n (parse-token (nth toks 2))]
          (println (str/join " " (map str (mt-gen seed n)))))))))
(-main)
